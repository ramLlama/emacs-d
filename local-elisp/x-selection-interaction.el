;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various Functions to deal with the X Selection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yank-to-x-primary-selection ()
  (interactive)
  (if (region-active-p)
      (progn
	(shell-command-on-region (region-beginning) (region-end) "xsel -i -p")
	(message "Yanked region to X PRIMARY selection!")
	(deactivate-mark))
    (message "No region active; can't yank to X PRIMARY selection!")))

(defun yank-to-x-secondary-selection ()
  (interactive)
  (if (region-active-p)
      (progn
	(shell-command-on-region (region-beginning) (region-end) "xsel -i -s")
	(message "Yanked region to X SECONDARY Selection!")
	(deactivate-mark))
    (message "No region active; can't yank to X SECONDARY selection!")))

(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
	(shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
	(message "Yanked region to X CLIPBOARD selection!")
	(deactivate-mark))
    (message "No region active; can't yank to X CLIPBOARD selection!")))

(provide 'x-selection-interaction)
