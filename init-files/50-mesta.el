(add-to-list 'auto-mode-alist '("\\.mt\\'" . mesta-mode))

(eval-after-load 'mesta-mode
  '(progn
     (add-hook 'mesta-mode-hook
          (lambda ()
            (setq fill-column 100)
            (add-hook 'before-save-hook
                      (lambda () (indent-region (point-min) (point-max)))
                      nil
                      t)))))

;; Mesta error regex
(defconst mesta-output-regexps
  '((file . "\\(.*\\)?")
    (line-column . "\\([[:digit:]]+\\)\.\\([[:digit:]]+\\)")))

(defconst mesta-compilation-regexp
  (let ((re (concat "^\\(?:error\\|\\(warning\\)\\|\\(note\\)\\):[^\0]+?--> "
                   (alist-get 'file mesta-output-regexps)
                   " "
                   (alist-get 'line-column mesta-output-regexps)
                   "-"
                   (alist-get 'line-column mesta-output-regexps))))
    (cons re '(3 (4 . 6) (5 . 7) (1 . 2) 3))))

;; adapted from rust-mode.el:rustc-scroll-down-after-next-error
(defun mesta-scroll-down-after-next-error ()
  "In the new style error messages, the regular expression
matches on the file name (which appears after `-->`), but the
start of the error appears a few lines earlier.  This hook runs
after `next-error' (\\[next-error]); it simply scrolls down a few lines in
the compilation window until the top of the error is visible."
  (save-selected-window
    (when (eq major-mode 'sml-mode)
      (select-window (get-buffer-window next-error-last-buffer 'visible))
      (when (save-excursion
              (beginning-of-line)
              (looking-at " *-->"))
        (let ((start-of-error
               (save-excursion
                 (beginning-of-line)
                 (while (not (looking-at "^[a-z]+"))
                   (forward-line -1))
                 (point))))
          (set-window-start (selected-window) start-of-error))))))

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist mesta-compilation-regexp)
     (add-hook 'next-error-hook 'mesta-scroll-down-after-next-error)))

;; MLton pacakges
(require 'esml-mlb-mode)
(require 'esml-du-mlton)
