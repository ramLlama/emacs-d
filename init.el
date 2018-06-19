;;;;;;;;;;;;;;;;;;;;
;; Early Settings ;;
;;;;;;;;;;;;;;;;;;;;

;; Override shell to use to bash. This is to fix any incompatibilities
;; with using fish
(setq shell-file-name "bash")

;;;;;;;;;;;;;;;;;;;;;;
;; Setup load paths ;;
;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/local-elisp")
(add-to-list 'load-path "~/.emacs.d/local-elisp.private")
(add-to-list 'custom-theme-load-path "~/.emacs.d/local-elisp/themes")

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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;
;; Load from init.d
;;

(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/init.d/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b7e273660390dfd7410df2b1740653cf13bb7072de1a4169ccfb073e3dc96786" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
