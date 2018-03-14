;;
;; Common for all programming modes
;;
(defun prog-mode-setup ()
  (setq fill-column 80)
  (auto-fill-mode 1)
  (flyspell-prog-mode)
  (column-number-mode 1))

(add-hook 'prog-mode-hook 'prog-mode-setup)
