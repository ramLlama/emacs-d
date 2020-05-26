(require 'whitespace)

(setq whitespace-style '(face trailing lines empty tabs tab-mark))
(face-spec-set 'whitespace-empty
               '((t (:background "black" :foreground "red"))))
(face-spec-set 'whitespace-line
               '((t (:background "black"))))
(face-spec-set 'whitespace-tab
               '((t (:background "black"
                                 :foreground "magenta"
                                 :underline (:color foreground-color
                                                    :style wave)))))
(face-spec-set 'whitespace-trailing
               '((t (:background "black"
                                 :foreground "red"
                                 :underline t
                                 :weight bold))))


;; start whitespace-mode for all modes but _after_ the other
;; hooks have run, including the ones that set 'fill-column.
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (progn
              ;; from https://www.emacswiki.org/emacs/WhiteSpace to
              ;; support buffer-local fill-column
              (setq-local whitespace-line-column fill-column)
              (when (or (derived-mode-p 'prog-mode) (derived-mode-p 'text-mode))
                (whitespace-mode 1)))))
