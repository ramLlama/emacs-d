;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;;;;;;;;;;;;;;;;;;;
;; Early Settings ;;
;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Override shell to use to bash. This is to fix any incompatibilities
;; with using fish
(setq shell-file-name "bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add .emacs.d/local-elisp to load-path ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/local-elisp")

;;;;;;;;;;;;
;; el-get ;;
;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes/")

;; First load all packages to install
(load "el-get-to-install-all")

;; Then load the machine-specific one if it exists.
(ignore-errors (load "el-get-to-install-machine-specific"))

(el-get 'sync el-get-to-install)

;;;;;;;;;;;;;;;;;;;;
;; Global Options ;;
;;;;;;;;;;;;;;;;;;;;
;; get rid of graphical features
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

;; set tab width
(setq tab-width 4)

;; use whitespace indent
(setq indent-tabs-mode nil)

;; Set default directory to home (useful for servers that I start at
;; random cwd's)
(setq default-directory (concat (getenv "HOME") "/"))

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defconst auto-save-dir "~/.emacs.d/auto-save-dir/")
(make-directory auto-save-dir t)
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-dir t)))


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
(yas-global-mode)

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

;; Saner unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ": ")

;; X clipboard interaction
(require 'x-selection-interaction)

;; Alt/Meta to C-x C-m
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;; Faster word deletion.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; switch-window
(global-set-key (kbd "C-x o") 'switch-window)

;; buffer-move
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; smex
(setq smex-save-file "~/.emacs.d/.smex-items")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; magit
(global-set-key (kbd "C-x C-z") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; goto-last-change
(global-set-key (kbd "C-x /") 'goto-last-change)

;; MLton error regex
(add-to-list
 'compilation-error-regexp-alist
 '("^\\(Warning\\|Error\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)\\.$"
   2 3 4))

;; show-paren mode
(show-paren-mode 1)

;; enable functions
(mapc (lambda (function) (put 'function 'disabled nil))
      '('upcase-region 'downcase-region))

;;
;; Common for all programming modes
;;
(defun prog-mode-setup ()
  (setq fill-column 80)
  (auto-fill-mode 1)
  (flyspell-prog-mode)
  (column-number-mode 1)
  (fci-mode 1))
(add-hook 'prog-mode-hook 'prog-mode-setup)

;;
;; Spell-check configuration
;;
;; Taken from
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if RUN-TOGETHER
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      (setq args nil)))
    args
    ))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq ispell-extra-args (flyspell-detect-ispell-args t))

(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

;; Load Solarized themes
(load-theme 'solarized t)
(setq frame-background-mode 'dark)
(enable-theme 'solarized)

;;;;;;;;;;;;;;;;;;;
;; Mode-Specific ;;
;;;;;;;;;;;;;;;;;;;

;;
;; Flyspell
;;

;; Set flyspell predicate matcher for LaTeX-mode
(eval-after-load "flyspell"
  (lambda ()
    (put 'latex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)))

;;
;; C, C++
;;
;; Default is google-c-style
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; linux c mode with tabs
(require 'linux-tabs-c-style)

;; MLton C code style
(require 'mlton-c-style)

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
;; As I use org-mode for lists, use org-indent-mode and visual-line-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (visual-line-mode 1)
	    (flyspell-mode 1)))

(setq org-log-done 'time  ;; timestamp on completion
      org-todo-keywords '((sequence
			   "TODO(t)"
			   "WIP(p!)"
			   "WAITING(w!)"
			   "|"
			   "DONE(d)"
			   "MISSED(m)"
			   "PASSED-ON(a)"
			   "DISCARDED(i)")) ;; list of todo states
      org-startup-indented t) ;; turn on org-indent-mode as I think in trees

;; Set org-mode file associations
(eval-after-load "org"
  (lambda ()
    ;; .txt files aren't in the list initially, but in case that changes
    ;; in a future version of org, use if to avoid errors
    (if (assoc "\\.pdf\\'" org-file-apps)
	(setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")
      (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s") t))
    (if (assoc "\\.png\\'" org-file-apps)
	(setcdr (assoc "\\.png\\'" org-file-apps) "firefox %s")
      (add-to-list 'org-file-apps '("\\.png\\'" . "firefox %s") t))
    (if (assoc "\\.jpg\\'" org-file-apps)
	(setcdr (assoc "\\.jpg\\'" org-file-apps) "firefox %s")
      (add-to-list 'org-file-apps '("\\.jpg\\'" . "firefox %s") t))
    (if (assoc "\\.jpeg\\'" org-file-apps)
	(setcdr (assoc "\\.jpeg\\'" org-file-apps) "firefox %s")
      (add-to-list 'org-file-apps '("\\.jpeg\\'" . "firefox %s") t))))

;; Enable speed keys
(setq org-use-speed-commands t)

;; keybindings
(eval-after-load "org"
  (lambda ()
    (org-defkey org-mode-map (kbd "C-c i") 'org-insert-heading)
    (org-defkey org-mode-map (kbd "C-c a") 'org-agenda)))

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

(add-hook 'LaTeX-mode-hook (lambda ()
			     (auto-fill-mode 1)
			     (setq ispell-parser 'tex)
			     (flyspell-mode)
			     (LaTeX-math-mode 1)
			     (TeX-source-correlate-mode 1)
			     (turn-on-reftex)
			     (LaTeX-add-environments
			      '("lemma" LaTeX-env-label)
			      '("lem" LaTeX-env-label)
			      '("theorem" LaTeX-env-label)
			      '("thm" LaTeX-env-label))))

(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

; Use 'auctex' as the automatic style save dir
(setq TeX-auto-local "auctex")

; Better error parsing
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

; Add "make" to command list
(eval-after-load "tex"
  (lambda ()
    (add-to-list 'TeX-command-list '("Make-TeX-Output" "make AUCTEX=1" TeX-run-TeX nil))
    (auctex-latexmk-setup)))

(setq reftex-ref-style-default-list (quote ("Varioref" "Hyperref" "Cleveref")))

;;
;; RefTeX
;;
(setq reftex-ref-style-default-list (quote ("Varioref" "Hyperref" "Cleveref")))
(setq reftex-label-alist
      '(("lemma"   ?l "lem:"  "~\\ref{%s}" nil ("lemma" "lem.") -3)
	("lem"   ?l "lem:"  "~\\ref{%s}" nil ("lemma" "lem.") -3)
	("theorem" ?t "thm:" "~\\ref{%s}" t   ("theorem" "thm.") -3)
	("thm" ?t "thm:" "~\\ref{%s}" t   ("theorem" "thm.") -3)))

;;
;; SCSS-mode settings
;;
(setq scss-compile-at-save nil)

;;
;; SML
;;
;; sml-mode turns indent-tabs-mode back on for some reason...
(add-hook 'sml-mode-hook (lambda ()
			   (setq indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))

;;
;; esml-mlb-mode
;;
(add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))

;;
;; scala-mode2 settings
;;
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;
;; systemtap-mode
;;
(add-to-list 'auto-mode-alist '("\\.stp$" . systemtap-mode))

;;
;; lua-mode
;;
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;
;; web-mode
;;
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;
;; cperl-mode
;;
;; Replace use of perl-mode with cperl-mode everywhere
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; Load prog-mode-hooks for cperl
(add-hook 'cperl-mode-hook 'prog-mode-setup)

;;
;; adoc-mode
;;
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

;;
;; org-journal
;;
(eval-after-load "org-journal"
  (lambda ()
    (setq org-journal-dir "~/bitbucket/mine/journal/"
	  org-journal-date-prefix "#+TITLE: "
	  org-journal-time-prefix "* "
	  org-journal-file-format "%Y%m%d.org"
	  org-journal-file-pattern "[0-9]\\{8\\}\\.org\'")))

 ;; turn off org-indent-mode for journal
(add-hook 'org-journal-mode-hook (lambda () (org-indent-mode 0)))

;;
;; company-mode
;;
(add-hook 'after-init-hook
	  (lambda ()
	    (global-company-mode)
	    (add-to-list 'company-dabbrev-code-modes 'cperl-mode)))
(add-hook 'company-mode-hook
  (lambda ()
    (define-key company-active-map "\C-o" 'company-show-location)
    (define-key company-active-map "\C-w" nil)))

;;
;; doc-view mode
;;
(setq doc-view-resolution 200)

;;
;; ediff-mode
;;
;; allow -diff as command-line for vimdiff replacement
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;; saner ediff default
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;
;; ProofGeneral
;;
;; general options
(setq proof-splash-enable nil
      proof-electric-terminator-enable t)

;; coq-mode options
(setq coq-one-command-per-line nil
      coq-compile-before-require t)

;; Emacs-generated custom-set-variables
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "f208be98a1816ec7b061ec70b80bfa3d5dde886bfb44d60832ca8d209bde5f5a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(org-agenda-files
   (quote
    ("~/org/personal.org" "~/org/phd/dblab.org" "~/org/phd/parmem.org" "~/org/phd/parinc.org" "~/org/phd/biginc.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "Source Code Pro")))))
