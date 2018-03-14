;;
;; cperl-mode
;;
;; Replace use of perl-mode with cperl-mode everywhere
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; Load prog-mode-hooks for cperl
(add-hook 'cperl-mode-hook 'prog-mode-setup)
