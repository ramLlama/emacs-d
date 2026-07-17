#! /usr/bin/env bash

#
# Force a clean rebuild of all byte-compiled (.elc) and native-compiled (.eln)
# package artifacts.
#
# Why this exists: native-comp resolves an .eln by hashing the .el path and
# contents, so a stale .eln keeps winning every load even after its .elc is
# rebuilt. `M-x package-recompile-all' only deletes .elc files and never touches
# the eln cache, so it alone cannot fix an .eln that was compiled before one of
# its macro-providing dependencies was installed (e.g. magit's `incf' compiled
# to a bare function call because compat-31 landed ten minutes later).
#
# Reach for this when a package errors with `invalid-function' or
# `void-function' on a macro that plainly exists, or after upgrading Emacs.
#
# Requires bash 4.4+ and GNU find/realpath.

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: recompile-elisp.sh [-n] [-h]
  -n   dry run: report what would be deleted, then exit.
  -h   this help.

Env:
  EMACS_DIR   Emacs config dir to operate on (default: ~/.emacs.d)
  EMACS       Emacs binary to compile with (default: emacs)
EOF
}

if ((BASH_VERSINFO[0] < 4 || (BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 4))); then
  echo "needs bash 4.4+ (found ${BASH_VERSION}); on macOS: brew install bash" >&2
  exit 1
fi

EMACS_DIR="${EMACS_DIR:-$HOME/.emacs.d}"
EMACS="${EMACS:-emacs}"

dry_run=false
while getopts ":nh" opt; do
  case $opt in
    n) dry_run=true ;;
    h) usage; exit 0 ;;
    *) echo "unknown option: -$OPTARG" >&2; usage >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))
# This script deletes things; don't let a typo'd argument imply a real run.
[[ $# -eq 0 ]] || { echo "unexpected argument: $1" >&2; usage >&2; exit 2; }

command -v "$EMACS" >/dev/null || { echo "emacs not found: $EMACS" >&2; exit 1; }
[[ -d "$EMACS_DIR/elpa" ]] || { echo "no elpa/ under $EMACS_DIR -- wrong EMACS_DIR?" >&2; exit 1; }

# The eln cache defaults to <user-emacs-directory>/eln-cache but is overridable
# via `startup-redirect-eln-cache', so ask Emacs rather than assume.
#
# --init-directory is what makes EMACS_DIR real: it sets `user-emacs-directory',
# which is the base both for the default cache path and for any relative
# redirect. Without it, -Q reports ~/.emacs.d regardless of EMACS_DIR.
# early-init.el is the only supported place to call `startup-redirect-eln-cache',
# and -Q does NOT load it for us, so load it explicitly -- but tolerate a broken
# one (NOERROR), since a broken init is a likely reason to be running this.
#
# Emits NUL-separated: the system dir first, then each user cache. Per
# `native-comp-eln-load-path', the LAST entry holds the .eln files built with
# Emacs itself and must never be deleted; every earlier entry is a user cache.
# Relative entries are relative to `invocation-directory'.
mapfile -d '' -t eln_paths < <(
  EMACS_DIR="$EMACS_DIR" "$EMACS" -Q --batch --init-directory="$EMACS_DIR/" --eval '
    (progn
      (load (expand-file-name "early-init.el" user-emacs-directory) t t)
      (princ (expand-file-name (car (last native-comp-eln-load-path))
                               invocation-directory))
      (princ "\0")
      (dolist (dir (butlast native-comp-eln-load-path))
        (princ (expand-file-name dir invocation-directory))
        (princ "\0")))'
)

[[ ${#eln_paths[@]} -ge 1 ]] || { echo "could not read native-comp-eln-load-path from $EMACS" >&2; exit 1; }
system_eln=$(realpath -m "${eln_paths[0]}")

# Each cache holds .eln files one level down, in a `comp-native-version-dir'
# subdirectory (e.g. 30.2-41d7827b), so stale subdirs from older Emacs versions
# get cleared out along with the current one.
targets=()
for dir in "${eln_paths[@]:1}"; do
  # Canonicalize before vetting: the checks below constrain the real target, not
  # a name that could symlink out of $HOME.
  dir=$(realpath -m "$dir")
  [[ "$dir" != "$system_eln" ]] || { echo "refusing to delete Emacs's system eln dir: $dir" >&2; exit 1; }
  case "$dir" in
    "$HOME"/*) ;;
    *) echo "refusing to delete eln cache outside \$HOME: $dir" >&2; exit 1 ;;
  esac
  [[ -d "$dir" ]] && targets+=("$dir")
done

elc_count=$(find "$EMACS_DIR" -name '*.elc' | wc -l)
eln_count=0
[[ ${#targets[@]} -gt 0 ]] && eln_count=$(find "${targets[@]}" -name '*.eln' | wc -l)
echo "elc: $elc_count under $EMACS_DIR"
echo "eln: $eln_count under ${targets[*]:-(no existing cache)}"

if $dry_run; then
  echo "dry run -- nothing deleted."
  exit 0
fi

# A running Emacs holds the old definitions in memory and will happily
# native-compile fresh .eln files underneath us; the rebuild is still correct,
# but the running session needs a restart to see it.
if pgrep -x emacs >/dev/null 2>&1; then
  echo "note: emacs is running -- restart it once this finishes."
fi

echo "deleting compiled artifacts..."
find "$EMACS_DIR" -name '*.elc' -delete
for dir in "${targets[@]}"; do
  rm -rf "${dir:?}"/*
done

echo "recompiling (this takes a few minutes)"

# package-recompile-all rebuilds every .elc. The .eln files are left to
# native-comp's JIT, which regenerates them lazily as packages load -- correctly
# now, since every dependency is present on disk before anything compiles.
#
# Byte-compile warnings are noisy and expected; the markers keep them visually
# separate from this script's own output.
echo "----------------- package-recompile-all: begin -----------------"
status=0
EMACS_DIR="$EMACS_DIR" "$EMACS" -Q --batch --init-directory="$EMACS_DIR/" --eval '
  (progn
    (setq package-user-dir (expand-file-name "elpa" (getenv "EMACS_DIR")))
    (package-initialize)
    (package-recompile-all))' 2>&1 || status=$?
echo "------------------ package-recompile-all: end ------------------"

if [[ $status -ne 0 ]]; then
  echo "recompile FAILED (exit $status)" >&2
  exit "$status"
fi

# package-recompile-all traps per-package errors and still exits 0, so the exit
# status alone is not evidence the rebuild was clean.
echo
echo "done: $(find "$EMACS_DIR" -name '*.elc' | wc -l) .elc present. Restart Emacs."
echo "(.eln files regenerate in the background as packages load.)"
