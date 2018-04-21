;;
;; Common for all programming modes
;;
(defun prog-mode-setup ()
  (flyspell-prog-mode)
  (column-number-mode 1)
  (whitespace-mode 1))

(add-hook 'prog-mode-hook 'prog-mode-setup)
