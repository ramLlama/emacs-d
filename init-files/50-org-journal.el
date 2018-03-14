;;
;; org-journal
;;
(eval-after-load "org-journal"
  (lambda ()
    (setq org-journal-dir "~/git/mine/journal/"
	  org-journal-date-prefix "#+TITLE: "
	  org-journal-time-prefix "* "
	  org-journal-file-format "%Y%m%d.org"
	  org-journal-file-pattern "[0-9]\\{8\\}\\.org\'")))

 ;; turn off org-indent-mode for journal
(add-hook 'org-journal-mode-hook (lambda () (org-indent-mode 0)))
