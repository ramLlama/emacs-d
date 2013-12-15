;;;;;;;;;;;;
;; el-get ;;
;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(setq
 el-get-sources
 '(;; Useful tools and configurations
   (:name switch-window
	  :after (global-set-key (kbd "C-x o") 'switch-window))

   (:name buffer-move ; have to add your own keys
	  :after (progn
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex ; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit ; git meet emacs, and a binding
	  :after (progn
		   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change ; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x /") 'goto-last-change)))

   ;; Modes and Styles
   (:name google-c-style
          :after (progn
		   (add-hook 'c-mode-common-hook 'google-set-c-style)))

   (:name systemtap-mode
	  :type github
	  :pkgname "ramLlama/systemtap-mode":localname
	  :after (progn
		   (add-to-list 'auto-mode-alist '("\\.stp$" . systemtap-mode))))

   (:name lua-mode
	  :after (progn
		   (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))))

   (:name org-mode
	  :after (progn
		   (add-hook 'org-mode-hook
			     (lambda ()
			       (define-key org-mode-map (kbd "C-c i") 'org-insert-heading)
			       (define-key org-mode-map (kbd "C-c a") 'org-agenda)))))

   (:name emacs-color-theme-solarized
	  :type github
	  :pkgname "ramLlama/emacs-color-theme-solarized"
	  :after (progn
		   (add-to-list 'custom-theme-load-path (el-get-package-directory "emacs-color-theme-solarized"))))

   (:name ag
	  :type github
	  :pkgname "Wilfred/ag.el")))

(setq my-packages
      (append '(
		el-get           ; el-get is self-hosting
		switch-window    ; takes over C-x o
		vkill            ; Process view and killing
		auto-complete    ; complete as you type with overlays
		buffer-move      ; move buffers through frames
		smex             ; smart M-x
		magit            ; git integration
		goto-last-change ; what do you think?!
		yasnippet        ; snippets
		google-c-style
		systemtap-mode
		lua-mode
		org-mode
		emacs-color-theme-solarized
		protobuf-mode
		haskell-mode
		auctex
		ag
		lorem-ipsum
		scss-mode
		)))

(el-get 'sync my-packages)

;;;;;;;;;;;;;;;;;;;;
;; Global Options ;;
;;;;;;;;;;;;;;;;;;;;
;; Add .emacs.d/local-elisp to load-path
(add-to-list 'load-path "~/.emacs.d/local-elisp")

;; get rid of splash screen and set tab width
(setq inhibit-splash-screen t)
(setq tab-width 4)

;; use whitespace indent
(setq indent-tabs-mode nil)

;; Set default directory to home (useful for servers that I start at
;; random cwd's)
(setq default-directory (concat (getenv "HOME") "/"))

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir "~/.emacs.d/auto-save-dir/")

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
	  (if buffer-file-name
	      (concat "#" (file-name-nondirectory buffer-file-name) "#")
	    (expand-file-name
	     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(setq backup-directory-alist `(("." . "~/.emacs.d/backup-dir")))
(setq backup-by-copying-when-linked t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; PuTTY fix. Ugly. Bad. But it works. (Good)
(define-key global-map "\M-[1~" 'beginning-of-line)
(define-key global-map [select] 'end-of-line)

;; allow narrow to region
(put 'narrow-to-region 'disabled nil)

;; set yasnippet a global minor mode
(yas/global-mode)

;; Clear Buffer List every so often
;; midnight mode
(require 'midnight)
;;kill buffers if they were last disabled more than this seconds ago
(setq clean-buffer-list-delay-special (* 15 60))
(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")
;; run clean-buffer-list every 2 hours
(setq clean-buffer-list-timer (run-at-time t (* 2 3600) 'clean-buffer-list))
;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps
      '("^.*$"))
;; keep these buffer untouched
;; prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*" "*scratch*")
       clean-buffer-list-kill-never-buffer-names-init))

;; Override shell to use to bash. This is to fix any incompatibilities
;; with using fish
(setq shell-file-name "bash")

;; Saner unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ": ")

;;
;; Keybindings
;;

;; Alt/Meta to C-x C-m
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;; Faster word deletion.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;;;;;;;;;;;;;;;;;;
;; Mode-Specific ;;
;;;;;;;;;;;;;;;;;;;

;;
;; C, C++
;;
;; linux c mode with tabs
(require 'linux-tabs-c-style)

;; MLton code style
(require 'mlton-c-style)

;; Set:
;; 1) auto-fill-mode (at 80 chars)
;; 2) flyspell-prog-mode
;; 3) auto-complete-mode
;; 4) column-number-mode
;; upon c or c++ mode
(require 'auto-complete)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq fill-column 80)
	    (auto-fill-mode)
	    (flyspell-prog-mode)
	    (auto-complete-mode)
	    (column-number-mode)))

;; Use project-specific modes
(defun maybe-mlton-c-style ()
  (when (and buffer-file-name
	     (string-match "mlton" buffer-file-name))
    (c-set-style "MLton")))

(add-hook 'c-mode-hook 'maybe-mlton-c-style)

;;
;; Eshell
;;
(defun m-eshell-hook ()
  (define-key eshell-mode-map "\C-p" 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map "\C-n" 'eshell-next-matching-input-from-input)

  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
  )
(add-hook 'eshell-mode-hook 'm-eshell-hook)

;;
;; Org-mode
;;
;; timestamp on completion
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(p!)" "WAITING(w!)" "|" "DONE(d)" "MISSED(m)" "PASSED-ON(a)" "DISCARDED(i)")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("f208be98a1816ec7b061ec70b80bfa3d5dde886bfb44d60832ca8d209bde5f5a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(org-agenda-files (quote ("~/org/trc.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load Solarized themes without enabling
(load-theme 'solarized-light t t)
(load-theme 'solarized-dark t t)

;;
;; Mutt configuration
;;
(add-to-list 'auto-mode-alist '("mutt[^/]*\\'" . mail-mode))
(add-hook 'mail-mode-hook (lambda ()
			    (auto-fill-mode 1)
			    (flyspell-mode 1)
			    (local-set-key "\C-Xk" 'server-edit)))

;;
;; Predictive Mode Settings
;; ATTENTION! Make sure this section comes before any reference to
;; Predictive mode.
;;
;; (add-to-list 'load-path "~/.emacs.d/el-get/predictive/")
;; (add-to-list 'load-path "~/.emacs.d/el-get/predictive/latex/")
;; (set-default 'predictive-auto-add-to-dict t)
;; (setq predictive-auto-learn t
;;       predictive-add-to-dict-ask nil
;;       predictive-use-auto-learn-cache nil
;;       predictive-which-dict t
;;       completion-accept-or-reject-by-default (quote ((t . reject)))
;;       completion-how-to-resolve-old-completions 'reject
;;       predictive-local-auxiliary-file-directory "predictive/"
;;       ; Doesn't work =(
;;       ; predictive-use-buffer-local-dict t
;;       predictive-dict-autosave t)

;;
;; AUCTeX settings
;;
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'predictive-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-PDF-mode t)

; Use 'auctex' as the automatic style save dir
(setq TeX-auto-local "auctex")

; Use completion-backward-kill-word in Latex-mode to make sure that
; predictive mode doesn't bork the buffer
; (add-hook 'LaTeX-mode-hook '(lambda () (local-set-key "\C-w" 'completion-backward-kill-word)))

;;
;; SCSS-mode settings
;;
(setq scss-compile-at-save nil)
