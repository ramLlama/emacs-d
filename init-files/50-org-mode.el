;;
;; Org-mode
;;

;; Org-Mode options
(setq org-log-done 'time  ;; timestamp on completion
      org-todo-keywords '((sequence
                           "TODO(t)"
                           "WIP(p!)"
                           "WAITING(w!)"
                           "|"
                           "DONE(d)"
                           "DELETED(e)")) ;; list of todo states
      org-startup-indented t ;; turn on org-indent-mode as I think in trees
      org-use-speed-commands t ;; speed-keys are awesome!
      org-enforce-todo-dependencies t ;; dependency management
      org-agenda-dim-blocked-tasks t)

;; As I use org-mode for lists, use org-indent-mode and visual-line-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (visual-line-mode 1)
	    (flyspell-mode 1)))

;; keybindings
(eval-after-load "org"
  (lambda ()
    ;; File Associations
    ;; .txt files aren't in the list initially, but in case that changes
    ;; in a future version of org, use if to avoid errors
    (if (assoc "\\.pdf\\'" org-file-apps)
	(setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")
      (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s") t))
    (if (assoc "\\.png\\'" org-file-apps)
	(setcdr (assoc "\\.png\\'" org-file-apps) "firefox %s")
      (add-to-list 'org-file-apps '("\\.png\\'" . "firefox %s") t))
    (if (assoc "\\.jpg\\'" org-file-apps)
	(setcdr (assoc "\\.jpg\\'" org-file-apps) "firefox %s")
      (add-to-list 'org-file-apps '("\\.jpg\\'" . "firefox %s") t))
    (if (assoc "\\.jpeg\\'" org-file-apps)
	(setcdr (assoc "\\.jpeg\\'" org-file-apps) "firefox %s")
    (add-to-list 'org-file-apps '("\\.jpeg\\'" . "firefox %s") t))

    ;; Keybindings
    (org-defkey org-mode-map (kbd "C-c i") 'org-insert-heading)
    (org-defkey org-mode-map (kbd "C-c a") 'org-agenda)))
