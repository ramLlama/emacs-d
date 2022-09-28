(with-eval-after-load 'python
  (require 'blacken)
  (add-hook 'python-mode-hook
            (lambda ()
              (progn (setq fill-column 100))))
  (add-hook 'python-mode-hook 'blacken-mode)
  (add-hook 'python-mode-hook 'eglot-ensure))
