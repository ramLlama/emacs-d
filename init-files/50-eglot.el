;;
;; Eglot
;;

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-hook 'eglot-managed-mode-hook (lambda () (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose))))
