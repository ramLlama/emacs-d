(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add MLton C style (equivalent to linux style)
            (c-add-style
             "mlton-c-style"
             '("google-c-style"
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)))))

(provide 'mlton-c-style)
