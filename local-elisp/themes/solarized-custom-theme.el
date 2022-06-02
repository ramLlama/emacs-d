;;; solarized-customizations.el --- emacs-color-theme-solarized customizations

;;; Commentary:
;; My customizations for emacs-color-theme-solarized faces.


;;; Code:

(require 'solarized-theme)

(defun solarized-definitions-custom ()
  `(
    ;; magit
    (magit-hash (,@fg-violet))
    (magit-section-highlight (,@bg-base02))
    (magit-diff-hunk-heading (:inherit magit-diff-file-heading))
    (magit-diff-context (,@fg-base01))
    (magit-diff-added (,@bg-base03 :inherit diff-added))
    (magit-diff-removed (,@bg-base03 :inherit diff-removed))
    (magit-diff-hunk-heading-highlight (:inherit magit-diff-file-heading-highlight))
    (magit-diff-context-highlight (,@bg-base02 :inherit magit-diff-context))
    (magit-diff-added-highlight (,@bg-base02 :inherit magit-diff-added))
    (magit-diff-removed-highlight (,@bg-base02 :inherit magit-diff-removed))

    ;; web-mode
    (web-mode-html-attr-name-face (,@fg-yellow))
    (web-mode-html-attr-value-face (,@fg-green))
    (web-mode-html-tag-bracket-face (,@fg-base0))
    (web-mode-html-tag-face (,@fg-base01))

    ;; cperl-mode
    (cperl-array-face (,@fg-violet))
    (cperl-hash-face (,@fg-magenta))
    (cperl-nonoverridable-face (:weight bold))

    ;; markup-faces (used by adoc-mode)
    (markup-list-face (,@bg-base02 ,@fg-yellow :inherit markup-meta-face))
    (markup-meta-face (,@fg-base00 :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monospace"))
    (markup-meta-hide-face (,@fg-base01 :height 0.8 :inherit markup-meta-face))
    (markup-typewriter-face (,@fg-cyan :inherit (fixed-pitch markup-gen-face)))
    (markup-verbatim-face (,@bg-base02))

    ;; tuareg-mode
    (tuareg-font-lock-governing-face (,@fg-green))
    (tuareg-font-lock-operator-face (,@fg-base0))

    ;; AUCTeX
    (font-latex-sedate-face (,@fg-base00))

    ;; ProofGeneral
    (proof-locked-face (,@fg-base01))
    (proof-eager-annotation-face (,@fmt-bold ,@fg-orange))

    ;; Smerge
    (smerge-upper (,@bg-base03))
    (smerge-lower (,@bg-base03))
    (smerge-refined-removed (,@bg-base02 ,@fg-red))
    (smerge-refined-added (,@bg-base02 ,@fg-green))
    )
  )

(create-solarized-theme
 solarized-custom
 "Custom version of sellout/emacs-color-theme-solarized"
 (solarized-color-definitions 'solarized-definitions-custom))

;;; solarized-customizations.el ends here
