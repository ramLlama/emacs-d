(require 'ansi-color)

(add-hook 'compilation-filter-hook
          (lambda ()
            (when (eq major-mode 'compilation-mode)
              (ansi-color-apply-on-region compilation-filter-start
                                          (point-max)))))
