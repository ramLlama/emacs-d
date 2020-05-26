;;
;; SML
;;


(require 'sml-mode)
(add-hook 'sml-mode-hook (lambda ()
                           (setq
                            ;; sml-mode turns indent-tabs-mode back on for some reason...
                            indent-tabs-mode nil
                            sml-indent-level 2
                            fill-column 100)))
(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))
