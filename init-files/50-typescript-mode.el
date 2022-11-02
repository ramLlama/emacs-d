;;
;; Typescript
;;

(with-eval-after-load 'typescript-mode
  (add-hook 'typescript-mode-hook (lambda ()
                                    (setq
                                     typescript-indent-level 2
                                     fill-column 100)
                                    (add-hook 'before-save-hook
                                              (lambda ()
                                                (indent-region (point-min) (point-max)))
                                              nil
                                              t)))
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'flymake-eslint-enable)
  (add-hook 'typescript-mode-hook 'prettier-mode))

;; Jenkinsfiles are also groovy files.
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
