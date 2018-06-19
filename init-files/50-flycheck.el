(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq-default flycheck-check-syntax-automatically '(save idle-change))
(setq-default flycheck-idle-change-delay 1.5)
