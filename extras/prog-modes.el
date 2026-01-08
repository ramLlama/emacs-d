;;; Ram's Customizations
;;;
;;; Extra config: Prog modes

;;; Contents:
;;;
;;;  - SML
;;;  - Python
;;;  - typescript
;;;  - json
;;;  - java
;;;  - Terraform
;;;  - C/C++

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

(use-package python
  :ensure t
  :after eglot
  :config
  (setopt python-indent-offset 4
          python-indent-guess-indent-offset nil
          python-indent-guess-indent-offset-verbose nil)
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) .
                 ,(eglot-alternatives
                   '(("ty" "server")
                     ("basedpyright-langserver" "--stdio")
                     "pylsp"
                     ("ruff"  "server"))))))

(use-package ruff-format
  :ensure t
  :hook (python-mode . ruff-format-on-save-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Typescript
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-ts-mode
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `((typescript-mode typescript-ts-mode tsx-mode tsx-ts-mode) .
                 ,(eglot-alternatives
                   '(("vtsls" "--stdio")
                     ("typescript-language-server"  "--stdio")))))
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package prettier
  :ensure t
  :hook (typescript-mode . prettier-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   JSON
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package jq-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Java
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot-java
  :ensure t
  :config
  (setopt eglot-java-eclipse-jdt-cache-directory (ramllama/local-dir "eglot-java/jdt-cache"))
  (setopt eglot-java-server-install-dir (ramllama/local-dir "eglot-java/jdtls-install"))
  (defun eglot-java--project-try (dir)
    nil)
  :hook ((java-mode . eglot-java-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Terraform
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   CSV
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package csv-mode
  :ensure t
  :config
  (setq csv-separators '("," ";" ":" "|" "\t"))
  :mode ("\\.csv\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   C/C++
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clang-format
  :ensure t
  :hook ((c-mode c++-mode) . clang-format-on-save-mode))

(use-package cmake-ts-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
