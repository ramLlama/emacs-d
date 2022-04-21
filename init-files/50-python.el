(with-eval-after-load 'python
  (require 'python-black)
  (add-hook 'python-mode-hook
            (lambda ()
              (progn (setq fill-column 100)
                     (python-black-on-save-mode))))
  (add-hook 'python-mode-hook 'eglot-ensure))
