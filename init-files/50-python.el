(require 'python-black)

(add-hook 'python-mode-hook
          (lambda ()
            (progn (setq fill-column 100)
                   (python-black-on-save-mode))))
