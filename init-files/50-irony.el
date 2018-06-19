;;
;; irony
;;
(with-eval-after-load 'irony-cdb
  (setq irony-cdb-compilation-databases
        (delete 'irony-cdb-json  irony-cdb-compilation-databases)))
(with-eval-after-load 'irony
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook (lambda () (irony-cdb-autosetup-compile-options)))

;; hooks into other modes
(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
(eval-after-load "company" '(add-to-list 'company-backends 'company-irony))
(add-hook 'c-mode-hook (lambda ()
                         (irony-mode)
                         (irony-eldoc)))
(add-hook 'c++-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)))
