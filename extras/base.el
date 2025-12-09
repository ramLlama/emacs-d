;;; Emacs Bedrock
;;;
;;; Extra config: Base enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.

;;;
;;; The consult package in particular has a vast number of functions that you
;;; can use as replacements to what Emacs provides by default. Please see the
;;; consult documentation for more information and help:
;;;
;;;     https://github.com/minad/consult
;;;
;;; In particular, many users may find `consult-line' to be more useful to them
;;; than isearch, so binding this to `C-s' might make sense. This is left to the
;;; user.

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Window management
;;;  - Power-ups: Embark and Consult
;;;  - Minibuffer and completion
;;;  - emacs server settings
;;;  - Misc. editing enhancements
;;;  - globally useful configurations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :custom (xterm-mouse-mode 1))

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c l" . avy-goto-line)
         ("C-c c" . avy-goto-char-timer)))

(use-package goto-last-change
  :ensure t
  :demand t
  :bind (("C-c /" . goto-last-change)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Window management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; custom split-window-sensibly
;; Adapted from https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(defun ramllama/split-window-sensibly (&optional window)
  (setq window (or window (selected-window)))
  (or (and (window-splittable-p window t)
           ;; Split window horizontally.
           (split-window window nil 'right))
      (and (window-splittable-p window)
           ;; Split window vertically.
           (split-window window nil 'below))
      (and (eq window (frame-root-window (window-frame window)))
           (not (window-minibuffer-p window))
           ;; If WINDOW is the only window on its frame and is not the
           ;; minibuffer window, try to split it horizontally disregarding the
           ;; value of `split-width-threshold'.
           (let ((split-width-threshold 0))
             (when (window-splittable-p window t)
               (split-window window nil 'right))))))

(use-package window
  :config
  (setq-default split-height-threshold  120
                split-width-threshold   160)
  (setopt split-window-preferred-function 'ramllama/split-window-sensibly))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop

         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s s" . consult-line)       ; consult-line instead of isearch
         ("M-s o" . consult-outline)
         )

  :custom
  ;; Narrowing lets you restrict results to certain groups of candidates
  (consult-narrow-key "<")
  (completion-in-region-function #'consult-completion-in-region))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act)
         ("C-c e" . embark-export))
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;; cross-terminal/ssh/tmux/host clipboard
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))


;; completion config
(use-package emacs
  :config (global-completion-preview-mode)
  :bind
  (:map completion-preview-active-mode-map
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate)))

(use-package emacs
  :custom (completion-show-help nil)
  :bind
  (:map completion-in-region-mode-map
        ("M-p" . minibuffer-previous-completion)
        ("M-n" . minibuffer-next-completion)
        ("RET" . minibuffer-choose-completion)))


;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; ;; Pretty icons for corfu
;; (use-package kind-icon
;;   :if (display-graphic-p)
;;   :ensure t
;;   :after corfu
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setopt completion-styles '(orderless)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Emacs server settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clear Buffer List every so often
(use-package midnight
  :ensure t
  :config
  (midnight-delay-set 'midnight-delay 0)            ;; midnight is midnight
  (setopt midnight-period (* 4 60 60))                ;; but run it every 4 hours
  (setopt clean-buffer-list-delay-special (* 15 60))  ;; buffer last touched delta
  (setopt clean-buffer-list-kill-regexps '("^.*$"))   ;; keep these buffers
  (setopt clean-buffer-list-kill-never-buffer-names
        '("*Messages*" "*scratch*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. editing enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setopt wgrep-auto-save-buffer t))

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; modern fill column
(setopt fill-column 100)

;; C-w dwim
(defun ramllama/cw-dwim ()
  "kill-region when region is action, backwards-kill-word otherwise"
  (interactive)
  (if (use-region-p)
      (kill-region 0 0 t)
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'ramllama/cw-dwim)

(use-package vundo
  :ensure t
  :config (setopt vundo-glyph-alist vundo-unicode-symbols))

(use-package multiple-cursors
  :ensure t)

;; allow -diff as command-line for vimdiff replacement
(defun ramllama/command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(use-package ediff
  :ensure t
  :config
  (add-to-list 'command-switch-alist '("-diff" . ramllama/command-line-diff))
  (setopt ediff-diff-options "-w")
  (setopt ediff-split-window-function 'split-window-horizontally)
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   globally useful configurations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config (setopt tab-width 4))

(use-package isearch
  :config (setopt isearch-lazy-count t
                  isearch-lazy-highlight t
                  isearch-allow-scroll 'unlimited
                  search-whitespace-regexp ".*?"))

(use-package jinx
  :ensure t
  :hook ((prog-mode . jinx-mode)
         (conf-mode-mode . jinx-mode)
         (text-mode . jinx-mode)))

(use-package emacs
  :hook
  (text-mode-hook . visual-wrap-prefix-mode))

(use-package emacs
  :config (global-hl-line-mode))
