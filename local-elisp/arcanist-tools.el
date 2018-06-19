;;; arcanist-tools.el --- Set of tools for working with Phabricator arcanist.

;;; Commentary:
;; Requires external helper scripts in PATH:
;;   - arc-get-tasks-elisp.py

;;; Code:

;; we could wrap it up nicely:
(defun arcanist-tools--call-process (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun arcanist-tools-insert-task ()
  "Prompts user to choose a Maniphest task to insert number for."
  (interactive)
  (let* ((response (arcanist-tools--call-process "arc-get-tasks-elisp.py"))
         (retcode (car response)))
    (if (= 0 retcode)
        (let* ((tasks (car (read-from-string (cadr response))))
               (chosen-task
                (completing-read "Choose Maniphest Task: " tasks nil t)))
          (insert (cdr (assoc-string chosen-task tasks))))
      (message "arc failed! Are you in the gitrepo?"))))

(defun arcanist-tools-insert-diff ()
  "Prompts user to choose a Maniphest task to insert number for."
  (interactive)
  (let* ((response (arcanist-tools--call-process
                    "arc-get-active-diffs-elisp.py"))
         (retcode (car response)))
    (if (= 0 retcode)
        (let* ((tasks (car (read-from-string (cadr response))))
               (chosen-task
                (completing-read "Choose Differential: " tasks nil t)))
          (insert (cdr (assoc-string chosen-task tasks))))
      (message "arc failed! Are you in the gitrepo?"))))

(provide 'arcanist-tools)

;;; arcanist-tools.el ends here
