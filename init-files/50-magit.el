;; magit
(require 'with-editor)
(require 'magit)
(global-set-key (kbd "C-x C-z") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")
(with-eval-after-load 'git-commit-mode
  (add-to-list 'git-commit-mode-hook 'flyspell-mode))
