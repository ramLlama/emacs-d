7;;;;;;;;;;;;
;; el-get ;;
;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t) (with-current-buffer
                                    (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
                                  (end-of-buffer)
                                  (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(setq
 el-get-sources
 '(;; Useful tools and things
   (:name buffer-move ; have to add your own keys
	  :after (lambda ()
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex ; a better (ido like) M-x
	  :after (lambda ()
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit ; git meet emacs, and a binding
	  :after (lambda ()
		   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change ; move pointer back to last change
	  :after (lambda ()
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x /") 'goto-last-change)))

   ;; Modes and Styles
   (:name google-c-style
          :after (lambda ()
		   (add-hook 'c-mode-common-hook 'google-set-c-style)))

   (:name systemtap-mode
	  :type http
	  :url "http://coderepos.org/share/export/39113/lang/elisp/systemtap-mode/systemtap-mode.el"
	  :localname "systemtap-mode.el"
	  :autoloads nil
	  :features systemtap-mode
	  :after (lambda ()
		   (add-to-list 'auto-mode-alist '("\\.stp$" . systemtap-mode))))

   (:name lua-mode
	  :after (lambda ()
		   (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))))

   (:name org-mode
	  :after (lambda ()
		   (add-hook 'org-mode-hook
			     (lambda ()
			       (define-key org-mode-map (kbd "C-c i") 'org-insert-heading)))))))

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
		)))

(el-get 'sync my-packages)

;;;;;;;;;;;;;;;;;;;;
;; Global Options ;;
;;;;;;;;;;;;;;;;;;;;
;; Add .emacs.d to load-path
(add-to-list 'load-path "~/.emacs.d/local-elisp")

;; get rid of splash screen and set tab width
(setq inhibit-splash-screen t)
(setq tab-width 4)

;; use whitespace indent
(setq indent-tabs-mode nil)

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

;; get yasnippet and auto-complete working
(require 'ac-yas)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(setq yas/trigger-key "TAB")

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
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode nil)
  (setq tab-width 8))
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;
;; Eshell
;;
(defun m-eshell-hook ()
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)

  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
)
(add-hook 'eshell-mode-hook 'm-eshell-hook)

;;;;;;;;;;;;;;;;
;; Work (TRC) ;;
;;;;;;;;;;;;;;;;
;; header files in repo_root are c++ files, not c files!
(add-to-list 'auto-mode-alist '(".*/repo_root/.*\\.h$" . c++-mode))
