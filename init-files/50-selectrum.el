;; -*- lexical-binding: t -*-

(require 'selectrum)
(selectrum-mode 1)

;;
;; custom functions
;;

;; Ref: https://www.gnu.org/software/emacs/manual/html_node/eintr/yank.html
(defun yank-choose-pop (&optional arg)
  "Paste a previously killed string.
With just \\[universal-argument] as ARG, put point at beginning,
and mark at end.  Otherwise, put point at the end, and mark at
the beginning without activating it.

This is like `yank-pop'.  The differences are:

- This let you manually choose a candidate to paste.

- This doesn't delete the text just pasted if the previous
  command is `yank'."
  (interactive "P")
  (let* ((selectrum-should-sort nil)
         (text nil))
    (setq text
          (completing-read "Yank: "
                           (cl-remove-duplicates
                            kill-ring :test #'equal :from-end t)
                           nil 'require-match))
    (unless (eq last-command 'yank)
      (push-mark))
    (setq last-command 'yank)
    (setq yank-window-start (window-start))
    (when (and delete-selection-mode (use-region-p))
      (delete-region (region-beginning) (region-end)))
    (insert-for-yank text)
    (if (consp arg)
        (goto-char (prog1 (mark t)
                     (set-marker (mark-marker) (point) (current-buffer)))))))

(defun selectrum-switch-buffer+ ()
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         (candidates
          (let* ((cb (window-buffer
                      (minibuffer-selected-window)))
                 (bf (or (buffer-file-name cb) "")))
            (lambda (input)
              (let* ((buffers (mapcar #'buffer-name
                                      (cl-delete-if
                                       (lambda (buf)
                                         (eq buf cb))
                                       (buffer-list))))
                     (files (cl-delete-if (lambda (f) (string= f bf))
                                          (copy-sequence recentf-list)))
                     (candidates ()))
                (cond ((string-prefix-p " " input)
                       (setq input (substring input 1))
                       (setq candidates
                             (cl-delete-if-not
                              (lambda (name)
                                (string-prefix-p " " name))
                              buffers)))
                      ((string-prefix-p "b " input)
                       (setq input (substring input 2))
                       (setq candidates
                             (cl-delete-if
                              (lambda (name)
                                (string-prefix-p " " name))
                              buffers)))
                      ((string-prefix-p "f " input)
                       (setq input (substring input 2))
                       (setq candidates files))
                      (t
                       (setq candidates
                             (append
                              (cl-delete-if
                               (lambda (name)
                                 (string-prefix-p " " name))
                               buffers)
                              files))))
                `((candidates . ,candidates)
                  (input . ,input))))))
         (cand (selectrum--read "Switch to: " candidates)))
    (cond ((member cand recentf-list)
           (find-file cand))
          (t
           (switch-to-buffer cand)))))

;;
;; custom remappings
;;

(global-set-key (kbd "M-y") 'yank-choose-pop)
(global-set-key (kbd "C-x b") 'selectrum-switch-buffer+)
