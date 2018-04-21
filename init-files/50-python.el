(add-hook 'python-mode-hook
          (lambda ()
            (progn (setq fill-column 100)
                   (make-local-variable 'whitespace-line-column)
                   (setq whitespace-line-column 98)
                   ;; reload whitespace-mode
                   (whitespace-mode -1)
                   (whitespace-mode 1))))
