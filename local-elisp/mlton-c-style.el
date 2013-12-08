(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add MLton C style (equivalent to linux style)
            (c-add-style
             "mlton-c-style"
             '("linux"
	       (indent-tabs-mode . nil)))))

(provide 'mlton-c-style)
