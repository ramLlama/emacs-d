;; coq-mode options
(setq coq-one-command-per-line nil
      coq-compile-before-require t
      coq-compile-parallel-in-background t)

(with-eval-after-load 'coq
  ;; The most common command by far. Having a 3(!)
  ;; keys long sequence for this command is just a
  ;; crime.
  (define-key coq-mode-map "\M-n"
    #'proof-assert-next-command-interactive))
