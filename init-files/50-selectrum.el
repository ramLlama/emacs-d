;; -*- lexical-binding: t -*-

(require 'selectrum)
(selectrum-mode 1)

;;
;; custom functions
;;

(defun yank-pop+ (&optional arg)
 "Call `yank-pop' with ARG when appropriate, or offer completion."
 (interactive "*P")
 (if arg (yank-pop arg)
   (let* ((old-last-command last-command)
          (selectrum-should-sort-p nil)
          (enable-recursive-minibuffers t)
          (text (completing-read
                 "Yank: "
                 (cl-remove-duplicates
                  kill-ring :test #'string= :from-end t)
                 nil t nil nil))
          ;; Find `text' in `kill-ring'.
          (pos (cl-position text kill-ring :test #'string=))
          ;; Translate relative to `kill-ring-yank-pointer'.
          (n (+ pos (length kill-ring-yank-pointer))))
     (unless (string= text (current-kill n t))
       (error "Could not setup for `current-kill'"))
     ;; Restore `last-command' over Selectrum commands.
     (setq last-command old-last-command)
     ;; Delegate to `yank-pop' if appropriate or just insert.
     (if (eq last-command 'yank)
         (yank-pop n) (insert-for-yank text)))))

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
         (cand (selectrum-read "Switch to: " candidates)))
    (cond ((member cand recentf-list)
           (find-file cand))
          (t
           (switch-to-buffer cand)))))

;;
;; custom remappings
;;

(global-set-key (kbd "M-y") 'yank-pop+)
(global-set-key (kbd "C-x b") 'selectrum-switch-buffer+)
