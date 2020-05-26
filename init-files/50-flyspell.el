;;
;; Flyspell
;;

;; Set flyspell predicate matcher for LaTeX-mode
(eval-after-load "flyspell"
  (lambda ()
    (put 'latex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)))

(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
