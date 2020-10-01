(require 'ctrlf)

;; set default bindings to fuzzy searches
(setf (alist-get "C-s" ctrlf-mode-bindings nil nil 'equal) 'ctrlf-forward-fuzzy)
(setf (alist-get "C-r" ctrlf-mode-bindings nil nil 'equal) 'ctrlf-backward-fuzzy)
(setf (alist-get "C-M-s" ctrlf-mode-bindings nil nil 'equal) 'ctrlf-forward-fuzzy-regexp)
(setf (alist-get "C-M-r" ctrlf-mode-bindings nil nil 'equal) 'ctrlf-backward-fuzzy-regexp)

(ctrlf-mode 1)
