;;
;; js2-mode
;;
;; Replace use of js-mode with js2-mode everywhere
(mapc
 (lambda (pair)
   (if (member (cdr pair) '(js-mode javascript-mode))
       (setcdr pair 'js2-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; Load prog-mode-hooks for js2
(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook (lambda ()
                                    (setq
                                     js-indent-level 2
                                     fill-column 100)
                                    (add-hook 'before-save-hook
                                              (lambda ()
                                                (indent-region (point-min) (point-max)))
                                              nil
                                              t))))
