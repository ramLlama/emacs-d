;;
;; irony
;;
(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)

            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)

            (irony-cdb-autosetup-compile-options)))

;; add to flycheck
(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)

(add-hook 'c-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)))

;; Enable irony for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook 'c++-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)))