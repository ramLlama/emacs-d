;; Change window separator to be prettier

(defun full-length-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?┃)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'full-length-window-divider)
