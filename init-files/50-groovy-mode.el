;;
;; Groovy
;;

(with-eval-after-load 'groovy-mode
  (add-hook 'groovy-mode-hook (lambda ()
                                (setq
                                 groovy-indent-offset 2
                                 fill-column 100)
                                (add-hook 'before-save-hook
                                          (lambda ()
                                            (indent-region (point-min) (point-max)))
                                          nil
                                          t))))

;; Jenkinsfiles are also groovy files.
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))
