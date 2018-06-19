;;
;; company-mode
;;
(add-hook 'after-init-hook
	  (lambda ()
	    (global-company-mode)
	    (add-to-list 'company-dabbrev-code-modes 'cperl-mode)
            (mapc (lambda (x) (setq company-backends
                                    (delete x company-backends)))
                    '(company-eclim
                      company-semantic
                      company-clang
                      company-xcode
                      company-cmake
                      company-oddmuse))))
(add-hook 'company-mode-hook
  (lambda ()
    (define-key company-active-map "\C-o" 'company-show-location)
    (define-key company-active-map "\C-w" nil)))
