;;
;; Mutt configuration
;;
(add-to-list 'auto-mode-alist '("mutt[^/]*\\'" . mail-mode))
(add-hook 'mail-mode-hook (lambda ()
			    (auto-fill-mode 1)
			    (flyspell-mode 1)
			    (local-set-key "\C-Xk" 'server-edit)))
