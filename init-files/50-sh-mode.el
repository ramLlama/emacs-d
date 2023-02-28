;;
;; sh-mode
;;
(add-hook 'sh-mode-hook 'flymake-mode)
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)
