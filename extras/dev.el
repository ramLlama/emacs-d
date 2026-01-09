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
;;;  - Templating
;;;  - Misc. Tools
;;;  - AI Tools

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Treesitter config
(use-package emacs
  :config
  ; Tell Emacs to prefer the treesitter mode. Also set each treesitter mode to run the original
  ; hooks. You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (dolist (mode-prefix '(yaml bash js2 typescript tsx json css python java c c++))
    (let ((orig-mode (intern (concat (symbol-name mode-prefix) "-mode")))
          (ts-mode (intern (concat (symbol-name mode-prefix) "-ts-mode")))
          (orig-mode-hook (intern (concat (symbol-name mode-prefix) "-mode-hook")))
          (ts-mode-hook (intern (concat (symbol-name mode-prefix) "-ts-mode-hook"))))
      (add-to-list 'major-mode-remap-alist (cons orig-mode ts-mode))
      (add-hook ts-mode-hook `(lambda () (run-hooks ',orig-mode-hook)))))
  :hook (prog-mode . (lambda () (kill-local-variable 'tab-width))))


(use-package compile
  :hook
  ; color support for compilation-mode
  (compilation-filter . (lambda ()
                                (when (eq major-mode 'compilation-mode)
                                  (ansi-color-apply-on-region compilation-filter-start
                                                              (point-max))))))

(use-package flymake
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "!" compilation-error)
     (warning "*" compilation-warning)
     (note "»" compilation-info))))

(use-package project
  :custom (project-mode-line t))  ; show project name in modeline


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(defun ramllama/show-github-url ()
  "Shows the GitHub URL for this file on the default branch on origin remote."
  (interactive)
  (let ((git-toplevel (magit-toplevel)))
    (if git-toplevel
        (let* ((origin-url (car (magit-git-lines "config" "--get" "remote.origin.url")))
               (origin-default-branch
                (or (when-let ((head (car (magit-git-lines "symbolic-ref"
                                                           "refs/remotes/origin/HEAD"
                                                           "--short"))))
                      (string-remove-prefix "origin/" head))
                    (when (zerop (magit-git-exit-code "rev-parse" "--verify" "origin/main"))
                      "main")
                    "master"))
             (www-url-prefix (save-match-data
                               (cond
                                ((string-match "^[^@]+@\\([^:]+\\):\\(.+?\\)\\(\\.git\\)?$" origin-url)
                                 (concat "https://"
                                         (match-string 1 origin-url)
                                         "/"
                                         (match-string 2 origin-url)))
                                ((string-match "^https://\\([^/]+\\)/\\(.+?\\)\\(\\.git\\)?$" origin-url)
                                 (concat "https://"
                                         (match-string 1 origin-url)
                                         "/"
                                         (match-string 2 origin-url))))))
             (www-url-suffix (string-remove-prefix git-toplevel (buffer-file-name)))
             (line-range (when (use-region-p)
                          (let ((start-line (line-number-at-pos (region-beginning)))
                                (end-line (line-number-at-pos (region-end))))
                            (if (= start-line end-line)
                                (format "#L%d" start-line)
                              (format "#L%d-L%d" start-line end-line)))))
             (www-url (concat www-url-prefix "/blob/" origin-default-branch "/" www-url-suffix line-range)))
          (kill-new www-url)
          (message "Copied to kill ring: %s" www-url))
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
;;;   Templating
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tempel
  :ensure t
  ;; By default, tempel looks at the file "templates" in
  ;; user-emacs-directory, but you can customize that with the
  ;; tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or
  ;; writing prose.
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. Tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package combobulate
  :ensure t
  :vc (:url "https://github.com/mickeynp/combobulate")
  :hook ((prog-mode . combobulate-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   AI Tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ramllama/api-key-cache nil
  "Cache for API keys as an alist of (key-name . key-value).")

(defun ramllama/clear-api-key-cache ()
  "Clear the API key cache."
  (interactive)
  (setq ramllama/api-key-cache nil)
  (message "API key cache cleared"))

(defun ramllama/query-api-key (key-name)
  "Query the user for an API key with caching."
  (interactive "sEnter key name: ")
  (let ((cached-key (alist-get key-name ramllama/api-key-cache nil nil #'string=)))
    (if cached-key
        cached-key
      (let ((key (read-string (format "Enter %s API key: " key-name))))
        (setq ramllama/api-key-cache (cons (cons key-name key) ramllama/api-key-cache))
        key))))

(use-package gptel
  :if (and (boundp 'ramllama/enable-gptel) 'ramllama/enable-gptel)
  :ensure t
  :config
  (gptel-make-gh-copilot "Copilot")
  (gptel-make-openai "OpenRouter"               ;Any name you want
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key (lambda () (ramllama/query-api-key "OpenRouter/coding-free"))
  :models '(microsoft/mai-ds-r1:free
            deepseek/deepseek-chat-v3-0324:free
            z-ai/glm-4.5-air:free))
  (gptel-make-ollama "Ollama [home-server]"
    :host "192.168.1.1:11434"
    :stream t
    :models '(hf.co/unsloth/Llama-3.1-8B-Instruct-GGUF:IQ4_XS
              hf.co/bartowski/DeepSeek-Coder-V2-Lite-Instruct-GGUF:Q4_K_M))
  (let ((el-files (directory-files
                   (expand-file-name "gptel-extras" user-emacs-directory)
                   t
                   "\\.el$")))
    (dolist (file el-files)
      (load file)))
  :bind ("C-x l" . gptel-menu))

;; dependency for claude-code
(use-package inheritenv
  :ensure t)

(use-package monet
  :if (and (boundp 'ramllama/enable-claude-code) ramllama/enable-claude-code)
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest)
  :ensure t
  :custom
  (monet-prefix-key nil)
  (monet-diff-tool #'monet-ediff-tool)
  (monet-diff-cleanup-tool #'monet-ediff-cleanup-tool))

(use-package claude-code
  :if (and (boundp 'ramllama/enable-claude-code) ramllama/enable-claude-code)
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :custom (claude-code-terminal-backend 'vterm)
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :bind-keymap ("C-x c" . claude-code-command-map))
