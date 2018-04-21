(require 'avy)

(eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-o") 'avy-isearch))
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-o") 'avy-goto-char-2)
