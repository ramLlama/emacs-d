;;;;;;;;;;;;;;;;;;;;
;; Global Options ;;
;;;;;;;;;;;;;;;;;;;;
;; get rid of graphical features
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

;; set tab width
(setq tab-width 4)

;; use whitespace indent
(setq-default indent-tabs-mode nil)

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

;; Faster word deletion.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
