# Style Guide

## General Elisp Conventions

### use-package Declarations

All third-party and most built-in package configuration uses `use-package`. Follow this pattern:

```elisp
(use-package PACKAGE-NAME
  :ensure t                    ; for packages from ELPA/MELPA
  :vc (:url "..." :rev :newest) ; for packages from GitHub
  :if CONDITION                ; for conditional loading
  :after OTHER-PACKAGE         ; load ordering
  :demand t                    ; force immediate load (use sparingly)
  :hook ((mode-a . function-a)
         (mode-b . function-b))
  :custom
  (option-name value)
  :config
  (imperative-setup-code)
  :bind (("KEY" . command)))
```

- Use `:ensure t` for packages from package archives.
- Use `:vc` for GitHub-hosted packages not on MELPA.
- Use `:custom` for setting user options (preferred over `setopt` inside `:config`).
- Use `:config` for imperative setup that must run after the package loads.
- Use `:hook` for mode hooks rather than `add-hook` when possible.
- Use `(use-package emacs ...)` as a convention for grouping built-in settings that do not belong to a specific package.

### Setting Variables

- Prefer `setopt` over `setq` for user options (defcustom variables). `setopt` runs the variable's setter function if one exists.
- Use `setq` or `setq-default` for non-customizable variables or buffer-local defaults.
- Use `defconst` for truly constant values (see backup-dir, auto-save-dir).

### Naming

- Custom functions: `ramllama/function-name` (lowercase, hyphen-separated)
- Custom variables: `ramllama/variable-name`
- Bedrock-original functions: `bedrock/function-name`
- No suffix conventions for predicates or boolean variables beyond Emacs conventions

### File Organization

Each extras file follows the same structural pattern:

```elisp
;;; Header comment with file purpose

;;; Contents:
;;;  - Section 1
;;;  - Section 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Section 1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(configuration-for-section-1)
```

- Use the full-width semicolon banner (80 chars of `;`) to delimit major sections.
- List all sections in a `Contents:` comment near the top of the file.
- Three semicolons (`;;;`) for top-level comments that describe sections.
- Single semicolon (`;`) for inline comments within code.

### Indentation and Formatting

- Spaces only, no tabs (Emacs default for Elisp).
- Standard Emacs Lisp indentation (as produced by `indent-region`).
- The config globally sets `indent-tabs-mode` to `nil` and `tab-width` to 4 for non-Elisp buffers.

### Keybinding Style

- Use `use-package` `:bind` keyword when binding keys for a specific package.
- Use `global-set-key` or `keymap-set` for standalone global bindings.
- Prefer `keymap-set` (newer API) when not inside a use-package form, though both styles appear in the codebase.

### Hook Patterns

Two styles are used:
1. Inside `use-package`: `:hook ((mode . function))` (preferred)
2. Standalone: `(add-hook 'mode-hook 'function)`

Lambda hooks are acceptable for short, single-use functions:
```elisp
:hook (prog-mode . (lambda () (kill-local-variable 'tab-width)))
```

### Eglot Server Configuration

Language server alternatives are configured using `eglot-alternatives` in `prog-modes.el`:

```elisp
(add-to-list 'eglot-server-programs
             `((mode-a mode-b) .
               ,(eglot-alternatives
                 '(("preferred-server" "arg")
                   ("fallback-server" "--stdio")))))
```

This pattern lets Eglot try servers in order and use whichever is available.

### Conditional Package Loading

Feature-flag pattern used for optional packages:

```elisp
(use-package PACKAGE
  :if (and (boundp 'ramllama/enable-FEATURE) ramllama/enable-FEATURE)
  ...)
```

The `boundp` check prevents errors when `local-init.el` has not defined the variable.
