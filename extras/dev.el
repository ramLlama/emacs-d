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
;;;  - LSP and DAP Tools
;;;  - Misc. Tools
;;;  - AI Tools

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
  (dolist (mode-prefix '(yaml bash js2 typescript json css python java c c++))
    (let ((orig-mode (intern (concat (symbol-name mode-prefix) "-mode")))
          (ts-mode (intern (concat (symbol-name mode-prefix) "-ts-mode")))
          (orig-mode-hook (intern (concat (symbol-name mode-prefix) "-mode-hook")))
          (ts-mode-hook (intern (concat (symbol-name mode-prefix) "-ts-mode-hook"))))
      (add-to-list 'major-mode-remap-alist (cons orig-mode ts-mode))
      (add-hook ts-mode-hook `(lambda () (run-hooks ',orig-mode-hook)))))
  :hook (prog-mode . (lambda () (kill-local-variable 'tab-width))))

;; color support for compilation-mode
(use-package compile
  :after ansi-color
  :hook (compilation-filter . (lambda ()
                                (when (eq major-mode 'compilation-mode)
                                  (ansi-color-apply-on-region compilation-filter-start
                                                              (point-max))))))

;; flymake config
(use-package flymake
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "!" compilation-error)
     (warning "*" compilation-warning)
     (note "»" compilation-info))))

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
                               (or (and (string-match "^git@\\([^:]+\\):\\(.+?\\)\\(\\.git\\)?$" origin-url)
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

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.css\\'" "\\.sass\\'" "\\.scss\\'"))


;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   LSP and DAP Tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for prog-mode.
  :hook
  ((prog-mode . eglot-ensure)
    (eglot-managed-mode . (lambda () (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (setopt eglot-events-buffer-config '(:size 2000000 :format full)
          eglot-report-progress t
          eglot-sync-connect 3  ; block 3 seconds to connect, then move to bg
          eglot-connect-timeout 60))  ; wait 60 seconds for connection

(use-package dape
  :ensure t
  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'gud)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. Tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package direnv
  :ensure t
  :config
  (direnv-mode))


(use-package combobulate
  :ensure t
  :vc (:url "https://github.com/mickeynp/combobulate")
  :hook ((prog-mode . combobulate-mode)))

(use-package vterm
  :ensure t
  :custom (vterm-max-scrollback 100000))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   AI Tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
  :ensure t
  :custom
  (copilot-idle-delay nil)
  (copilot-enable-predicates nil)
  :hook (((prog-mode) . copilot-mode))
  :bind (("C-x TAB" . copilot-complete)
         :map copilot-completion-map
         ("TAB" . copilot-accept-completion-by-line)))

(use-package gptel
  :ensure t
  :config (setopt gptel-model 'claude-sonnet-4
                  gptel-backend (gptel-make-gh-copilot "Copilot"))
  :bind ("C-x l" . gptel-menu))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :custom (claude-code-terminal-backend 'vterm)
  :config (claude-code-mode)
  :bind-keymap ("C-x c" . claude-code-command-map))
