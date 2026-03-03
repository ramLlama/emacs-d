# Architecture

## Load Order Diagram

```
Emacs startup
  |
  v
early-init.el
  - GC threshold raised to 10MB
  - Native-comp warnings silenced
  - Frame defaults: maximized, dark bg (#000000), no toolbar/menubar
  |
  v
init.el
  - Version guard (Emacs 29+)
  - Define ramllama/local-dir helper
  - Package archives: add MELPA, allow built-in upgrades
  - Basic settings: auto-revert, savehist, ace-window, windmove
  - Completion settings (basic style, before Orderless overrides it)
  - UI defaults: line numbers in prog-mode, visual-line in text-mode
  - Tab-bar disabled
  - Theme: modus-vivendi-tinted
  |
  +---> local-init.el  (gitignored, machine-specific)
  |       Sets feature flags, machine-local paths, etc.
  |
  +---> extras/base.el
  |       Motion: avy, goto-last-change
  |       Windows: custom split-window-sensibly
  |       Completion stack: Vertico, Marginalia, Orderless, Cape, Consult
  |       Embark (action framework)
  |       Clipetty (cross-terminal clipboard)
  |       Server: midnight buffer cleanup
  |       Editing: wgrep, vundo, multiple-cursors, ediff
  |       Misc: jinx (spellcheck), direnv, vterm, isearch enhancements
  |
  +---> extras/dev.el
  |       Tree-sitter mode remapping for all supported languages
  |       Compilation buffer ANSI color support
  |       Flymake (margin indicators)
  |       Magit (git), custom GitHub URL helper
  |       File type modes: markdown, yaml, json, web-mode
  |       Eglot (LSP, auto-enabled on prog-mode)
  |       Dape (DAP debugger)
  |       Tempel (snippet/template system)
  |       Combobulate (tree-sitter structural editing)
  |       AI tools (conditional):
  |         gptel -> loads all files in gptel-extras/
  |         claude-code.el + monet.el
  |         agent-shell
  |
  +---> extras/prog-modes.el
          SML: sml-mode, MLton error regexp
          Python: rass/ty/basedpyright/pylsp/ruff LSP chain, ruff-format
          TypeScript: rass/vtsls/ts-language-server, prettier
          JSON: jq-mode
          Java: eglot-java (project detection disabled)
          Terraform: terraform-mode
          CSV: csv-mode
          C/C++: clang-format, cmake-ts-mode
```

## Package Relationship Map

### Completion Stack

The completion system is layered:

1. **Vertico** -- Vertical minibuffer completion UI
2. **Orderless** -- Flexible matching style (overrides init.el's `completion-styles`)
3. **Marginalia** -- Rich annotations in completion candidates
4. **Consult** -- Enhanced commands (buffer switch, ripgrep, line search, yank-pop)
5. **Embark** -- Contextual actions on completion candidates
6. **Cape** -- Extra completion-at-point backends (dabbrev, file)
7. **completion-preview-mode** -- Inline completion preview (built-in Emacs 30 feature)

### LSP Stack

- **Eglot** (built-in) is the LSP client, auto-started via `prog-mode-hook`
- Server programs per language defined in `prog-modes.el` using `eglot-alternatives`
- **Flymake** shows diagnostics (configured for margin indicators)
- **Eldoc** displays documentation (strategy set to compose-eagerly under Eglot)

### AI Tool Stack (all conditional on feature flags)

- **gptel** -- LLM chat interface; backends: GitHub Copilot, OpenRouter, Ollama
- **gptel-extras/** -- Auto-loaded extensions (currently: context-aware rewrite directive)
- **claude-code.el** -- Claude Code terminal integration (uses vterm backend)
- **monet.el** -- Diff review tool for claude-code
- **agent-shell** -- Agent shell integration

### Tree-sitter Integration

- Mode remapping in `dev.el` maps legacy modes to `-ts-mode` variants
- Hook chaining ensures original mode hooks fire on ts-mode activation
- **Combobulate** adds structural editing on top of tree-sitter parse trees

## External Dependencies

- **ripgrep** (`rg`) -- Used by `consult-ripgrep`
- **direnv** -- Environment variable management per-project
- **Tree-sitter grammars** -- Must be installed per-language via `M-x treesit-install-language-grammar`
- **Language servers**: rass, basedpyright, pylsp, ruff, vtsls, typescript-language-server, clangd, jdtls
- **Formatters**: ruff (Python), prettier (TypeScript), clang-format (C/C++)
- **vterm** -- Requires cmake and libtool for compilation
- **jinx** -- Spellchecker, requires enchant2 library
- **mu** -- Required only if email.el is enabled
