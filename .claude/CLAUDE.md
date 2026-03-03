# emacs-d

## What This Project Does

Personal Emacs configuration for macOS, forked from [emacs-bedrock](https://sr.ht/~ashton314/emacs-bedrock/) and extended with custom additions. This is the user's `~/.emacs.d` directory, managed as a git repo at `~/repos/emacs-d`. It configures Emacs 29+ with tree-sitter, Eglot LSP, and a modern completion stack.

## Tech Stack

- **Emacs 29+** (requires 29 minimum; uses native compilation)
- **Emacs Lisp** (all configuration)
- **use-package** for package declarations (built into Emacs 29)
- **Tree-sitter** for syntax highlighting/parsing (preferred over legacy modes)
- **Eglot** for LSP (built-in, auto-enabled on all prog-mode buffers)
- **Package sources**: GNU ELPA, NonGNU ELPA, MELPA, and some `:vc` packages from GitHub

## Repository Structure

```
emacs-d/
  early-init.el          # Runs before init.el: GC tuning, frame setup, dark theme base
  init.el                # Main entry point: basic settings, then loads extras/
  local-init.el          # (gitignored) Machine-specific overrides, loaded before extras
  extras/
    base.el              # UI/UX enhancements: Vertico, Consult, Embark, Orderless, etc.
    dev.el               # Developer tools: Magit, Eglot, Dape, tree-sitter, AI tools
    prog-modes.el        # Language-specific configs: Python, TS, SML, Java, C/C++, etc.
    vim-like.el          # Evil mode (currently disabled in init.el)
    org.el               # Org-mode (currently disabled in init.el)
    email.el             # Email via mu4e (currently disabled in init.el)
    researcher.el        # Academic tools: citar, org-roam (currently disabled in init.el)
  gptel-extras/
    gptel-rewrite.el     # Context-aware rewrite directive for gptel
  elpa/                  # (gitignored) Installed packages
  eln-cache/             # (gitignored) Native-compiled elisp cache
  backup/                # (gitignored) Emacs backup files
  auto-saves/            # (gitignored) Emacs autosave files
  .local/                # (gitignored) Local data dirs (eglot-java cache, etc.)
```

## Load Order

1. `early-init.el` -- GC threshold, suppress native-comp warnings, frame defaults (dark bg, maximized, no toolbar/menubar)
2. `init.el` -- Core settings, then sequentially loads:
   1. `local-init.el` -- Machine-local overrides (gitignored; sets feature flags like `ramllama/enable-gptel`)
   2. `extras/base.el` -- Completion stack, motion aids, editing enhancements
   3. `extras/dev.el` -- Version control, LSP, DAP, AI tools, tree-sitter remapping
   4. `extras/prog-modes.el` -- Per-language configuration

## Key Concepts

### Namespace Convention

Custom functions and variables use the `ramllama/` prefix (e.g., `ramllama/split-window-sensibly`, `ramllama/query-api-key`). Some original bedrock functions use the `bedrock/` prefix.

### Feature Flags via local-init.el

Several features are conditionally loaded based on variables expected to be set in `local-init.el`:
- `ramllama/enable-gptel` -- Enables gptel (LLM chat in Emacs)
- `ramllama/enable-claude-code` -- Enables claude-code.el and monet.el
- `ramllama/enable-agent-shell` -- Enables agent-shell

### Tree-sitter Mode Remapping

In `dev.el`, all major modes for supported languages are remapped to their `-ts-mode` equivalents (e.g., `python-mode` -> `python-ts-mode`). The original mode hooks are also chained so they still fire on the ts-mode. You need to run `M-x treesit-install-language-grammar` for each language before tree-sitter modes work.

### Eglot Runs on All Prog Modes

`eglot-ensure` is hooked into `prog-mode`. This means any programming buffer will attempt to start a language server. Language-specific server programs are configured in `prog-modes.el`.

### Custom LSP Server: rass

Several languages list `"rass"` as the first-priority LSP server alternative (e.g., `("rass" "python")`, `("rass" "typescript")`). This appears to be a custom/personal LSP wrapper. If it is not available, Eglot falls back to the next alternative in the `eglot-alternatives` list.

### Helper: ramllama/local-dir

`(ramllama/local-dir NAME)` returns a path under `~/.emacs.d/.local/NAME`, creating it if needed. Used to keep tool-specific data out of the main config directory.

## Development Workflow

This is a dotfiles repo. Changes are made by editing `.el` files directly:

- **Test changes**: Evaluate individual forms with `C-x C-e`, or reload a whole file with `M-x load-file`.
- **Full restart test**: Restart Emacs to verify the complete load order works.
- **New packages**: Add a `use-package` declaration in the appropriate extras file. Use `:ensure t` for MELPA/ELPA packages, `:vc` for GitHub repos.
- **New language support**: Add configuration in `extras/prog-modes.el`.
- **Machine-specific config**: Put it in `local-init.el` (gitignored).

## Critical Idiosyncrasies and Gotchas

1. **local-init.el is gitignored and optional**: `init.el` loads it with `(load ... t)` (NOERROR), so Emacs starts cleanly even without the file. Feature flags it would set simply remain unbound (checked via `boundp`).

2. **Hardcoded ~/.emacs.d paths**: Backup and autosave directories are hardcoded to `~/.emacs.d/backup/` and `~/.emacs.d/auto-saves/` rather than using `user-emacs-directory`. This assumes the repo is symlinked or deployed to `~/.emacs.d`.

3. **elpa/ is gitignored**: Packages must be installed fresh on each machine. There is no lockfile mechanism.

4. **Disabled extras are still in the repo**: `vim-like.el`, `org.el`, `email.el`, and `researcher.el` are commented out in `init.el` but remain available for enabling.

5. **gptel-extras auto-loading**: All `.el` files in the `gptel-extras/` directory are loaded automatically when gptel initializes (via a `directory-files` + `dolist` loop in `dev.el`). Drop new files there and they take effect.

6. **eglot-java project detection is disabled**: `eglot-java--project-try` is overridden to return `nil`, preventing it from auto-detecting Java projects (probably to avoid false positives).

7. **project-mode-line is disabled**: The `project` package has `project-mode-line` set to `nil` because it can be very slow.

8. **setopt vs setq**: This config prefers `setopt` (Emacs 29+) over `setq` for user options, but a few places still use `setq` or `setq-default`.

## Context Files

- [Architecture Details](architecture.md) -- Load order, package relationships, and data flow
- [Style Guide](style-guide.md) -- Elisp conventions used in this config
