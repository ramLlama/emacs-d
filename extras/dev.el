;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode. Also set each treesitter mode to run the original hooks.
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (dolist (mode-prefix '(yaml bash js2 typescript json css python java))
    (let ((orig-mode (intern (concat (symbol-name mode-prefix) "-mode")))
          (ts-mode (intern (concat (symbol-name mode-prefix) "-ts-mode")))
          (orig-mode-hook (intern (concat (symbol-name mode-prefix) "-mode-hook")))
          (ts-mode-hook (intern (concat (symbol-name mode-prefix) "-ts-mode-hook"))))
      (add-to-list 'major-mode-remap-alist (cons orig-mode ts-mode))
      (add-hook ts-mode-hook `(lambda () (run-hooks ',orig-mode-hook))))))

;; color support for compilation-mode
(use-package compile
  :after ansi-color
  :hook (compilation-filter . (lambda ()
                                (when (eq major-mode 'compilation-mode)
                                  (ansi-color-apply-on-region compilation-filter-start
                                                              (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(defun show-github-url ()
  "Shows the GitHub URL for this file on the default branch on origin remote."
  (interactive)
  (let ((git-toplevel (magit-toplevel)))
    (if git-toplevel
        (let* ((origin-url (car (magit-git-lines "config" "--get" "remote.origin.url")))
               (origin-default-branch
                (string-remove-prefix "origin/"
                                      (car (magit-git-lines "symbolic-ref"
                                                            "refs/remotes/origin/HEAD"
                                                            "--short"))))
             (www-url-prefix (save-match-data
                               (or (and (string-match "^git@\\([^:]+\\):\\(.+?\\)\\.git$" origin-url)
                                    (concat "https://"
                                            (match-string 1 origin-url)
                                            "/"
                                            (match-string 2 origin-url)))
                                   (and (string-match "^https://\\([^/]+\\)/\\(.+?\\)\\(\\.git\\)?$" origin-url)
                                    (concat "https://"
                                            (match-string 1 origin-url)
                                            "/"
                                            (match-string 2 origin-url))))))
             (www-url-suffix (string-remove-prefix git-toplevel (buffer-file-name)))
             (www-url (concat www-url-prefix "/blob/" origin-default-branch "/" www-url-suffix)))
          (message www-url))
    (message "Not a gitrepo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for prog-mode.
  :hook
  (((prog-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (setopt eglot-report-progress nil)
  (add-hook 'eglot-managed-mode-hook (lambda () (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose)))
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )
