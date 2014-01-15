;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various Functions to deal with the X Clipboard ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
	(shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
	(message "Yanked region to clipboard!")
	(deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(provide 'x-clipboard-interaction)
