;;
;; Eshell
;;
(defun m-eshell-hook ()
  (define-key eshell-mode-map "\C-p" 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map "\C-n" 'eshell-next-matching-input-from-input)

  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
  )
(add-hook 'eshell-mode-hook 'm-eshell-hook)
