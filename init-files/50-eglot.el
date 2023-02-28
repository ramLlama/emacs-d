;;
;; Eglot
;;

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook (lambda () (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose))))
