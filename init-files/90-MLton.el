;; MLton error regex
(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\(Warning\\|Error\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)-[0-9]+\\.[0-9]+\\.$"
   2 3 4))

;; MLton pacakges
(require 'esml-mlb-mode)
(require 'esml-du-mlton)

;; set variables
(require 'sml-mode)
(setq sml-indent-level 2)

;; Set auto mode alist
(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))
