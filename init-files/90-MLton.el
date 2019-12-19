;; MLton error regex
(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\(Warning\\|Error\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)-[0-9]+\\.[0-9]+\\.$"
   2 3 4))

;; MLton pacakges
(require 'esml-mlb-mode)
(require 'esml-du-mlton)
