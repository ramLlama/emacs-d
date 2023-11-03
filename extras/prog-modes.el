;;; Ram's Customizations
;;;
;;; Extra config: Prog modes

;;; Contents:
;;;
;;;  - SML
;;;  - Python

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   SML
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sml-mode
  :mode ("\\.sml\\'" "/mlton[^/]*/.*\\.sig\\'" "/mlton[^/]*/.*\\.fun\\'")
  :ensure t
  :after compile
  :config
  (add-to-list 'compilation-error-regexp-alist
   '("^\\(Warning\\|Error\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)-[0-9]+\\.[0-9]+\\.$" 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Python
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blacken-mode
  :ensure t
  :after python-ts-mode
  :config
  (add-hook 'python-ts-mode-hook 'blacken-mode))
