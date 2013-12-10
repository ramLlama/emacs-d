(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add MLton C style (equivalent to linux style)
            (c-add-style
             "MLton"
             '("Google"
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)))))

(provide 'mlton-c-style)
