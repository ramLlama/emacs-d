(add-to-list 'auto-mode-alist '("\\.im\\'" . imli-mode))

(eval-after-load 'imli-mode
  '(progn
     (add-hook 'imli-mode-hook
          (lambda ()
            (setq fill-column 100)
            (add-hook 'before-save-hook
                      (lambda () (indent-region (point-min) (point-max)))
                      nil
                      t)))))

;; Imli error regex
(defconst imli-output-regexps
  '((file . "\\(.*\\)?")
    (line-column . "\\([[:digit:]]+\\)\.\\([[:digit:]]+\\)")))

(defconst imli-compilation-regexp
  (let ((re (concat "^\\(?:error\\|\\(warning\\)\\|\\(note\\)\\):[^\0]+?--> "
                   (alist-get 'file imli-output-regexps)
                   " "
                   (alist-get 'line-column imli-output-regexps)
                   "-"
                   (alist-get 'line-column imli-output-regexps))))
    (cons re '(3 (4 . 6) (5 . 7) (1 . 2) 3))))

;; adapted from rust-mode.el:rustc-scroll-down-after-next-error
(defun imli-scroll-down-after-next-error ()
  "In the new style error messages, the regular expression
matches on the file name (which appears after `-->`), but the
start of the error appears a few lines earlier.  This hook runs
after `next-error' (\\[next-error]); it simply scrolls down a few lines in
the compilation window until the top of the error is visible."
  (save-selected-window
    (when (eq major-mode 'imli-mode)
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
     (add-to-list 'compilation-error-regexp-alist imli-compilation-regexp)
     (add-hook 'next-error-hook 'imli-scroll-down-after-next-error)))

