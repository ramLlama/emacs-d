;;; Ram's Customizations
;;;
;;; Extra config: Prog modes

;;; Contents:
;;;
;;;  - SML
;;;  - Python
;;;  - typescript

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   SML
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sml-mode
  :ensure t
  :mode ("\\.sml\\'" "/mlton[^/]*/.*\\.sig\\'" "/mlton[^/]*/.*\\.fun\\'")
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
  :defer t
  :hook (python-mode . blacken-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Typescript
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'"))

(use-package prettier
  :ensure t
  :defer t
  :hook (typescript-mode . prettier-mode))
