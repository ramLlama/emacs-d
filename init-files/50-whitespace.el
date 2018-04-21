(require 'whitespace)

(setq whitespace-style '(face trailing lines empty tabs tab-mark))
(setq whitespace-line-column 78)
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
