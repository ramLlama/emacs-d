;;
;; company-mode
;;
(add-hook 'after-init-hook
	  (lambda ()
	    (global-company-mode)
	    (add-to-list 'company-dabbrev-code-modes 'cperl-mode)
	    (add-to-list 'company-backends 'company-irony)))
(add-hook 'company-mode-hook
  (lambda ()
    (define-key company-active-map "\C-o" 'company-show-location)
    (define-key company-active-map "\C-w" nil)))
