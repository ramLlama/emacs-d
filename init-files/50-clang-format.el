(require 'clang-format)
(add-hook 'before-save-hook
          (lambda () (when (eq major-mode 'c++-mode) (clang-format-buffer))))
