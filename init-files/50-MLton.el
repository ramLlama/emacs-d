;; Set auto mode alist
(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))

;; MLton error regex
(eval-after-load 'compile
  '(progn
     (add-to-list
      'compilation-error-regexp-alist
      '("^\\(Warning\\|Error\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)-[0-9]+\\.[0-9]+\\.$"
        2 3 4))))
