;;
;; SML
;;
;; sml-mode turns indent-tabs-mode back on for some reason...
(add-hook 'sml-mode-hook (lambda ()
			   (setq indent-tabs-mode nil
				 sml-indent-level 2)))

(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))
