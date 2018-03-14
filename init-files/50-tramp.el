;;
;; Tramp
;;
(eval-after-load "tramp"
  (lambda ()
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))
