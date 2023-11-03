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
  :ensure t
  :mode ("\\.sml\\'" "/mlton[^/]*/.*\\.sig\\'" "/mlton[^/]*/.*\\.fun\\'")
  :after compile
  :config
  (add-to-list 'compilation-error-regexp-alist
   '("^\\(Warning\\|Error\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)-[0-9]+\\.[0-9]+\\.$" 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Python
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blacken
  :ensure t
  :after python
  :hook (python-mode . blacken-mode))
