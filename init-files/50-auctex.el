;;
;; Predictive Mode Settings
;; ATTENTION! Make sure this section comes before any reference to
;; Predictive mode.
;;
;; (add-to-list 'load-path "~/.emacs.d/el-get/predictive/")
;; (add-to-list 'load-path "~/.emacs.d/el-get/predictive/latex/")
;; (set-default 'predictive-auto-add-to-dict t)
;; (setq predictive-auto-learn t
;;       predictive-add-to-dict-ask nil
;;       predictive-use-auto-learn-cache nil
;;       predictive-which-dict t
;;       completion-accept-or-reject-by-default (quote ((t . reject)))
;;       completion-how-to-resolve-old-completions 'reject
;;       predictive-local-auxiliary-file-directory "predictive/"
;;       ; Doesn't work =(
;;       ; predictive-use-buffer-local-dict t
;;       predictive-dict-autosave t)

;;
;; AUCTeX settings
;;
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-master nil)

(add-hook 'LaTeX-mode-hook (lambda ()
			     (auto-fill-mode 1)
			     (setq ispell-parser 'tex)
			     (flyspell-mode)
			     (LaTeX-math-mode 1)
			     (TeX-source-correlate-mode 1)
			     (turn-on-reftex)
			     (LaTeX-add-environments
			      '("lemma" LaTeX-env-label)
			      '("lem" LaTeX-env-label)
			      '("theorem" LaTeX-env-label)
			      '("thm" LaTeX-env-label)
			      '("rulearray" LaTeX-env-label))
                 (add-to-list 'font-latex-math-environments '"rulearray")))

(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

; Use 'auctex' as the automatic style save dir
(setq TeX-auto-local "auctex")

; Better error parsing
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

; Add "make" to command list
(eval-after-load "tex"
  (lambda ()
    (add-to-list 'TeX-command-list '("Make-TeX-Output" "make AUCTEX=1" TeX-run-TeX nil))
    (auctex-latexmk-setup)))

;;
;; RefTeX
;;
(setq reftex-ref-style-default-list (quote ("Varioref" "Hyperref" "Cleveref")))
(setq reftex-label-alist
      '(("lemma"   ?l "lem:"  "~\\ref{%s}" nil ("lemma" "lem.") -3)
	("lem"   ?l "lem:"  "~\\ref{%s}" nil ("lemma" "lem.") -3)
	("theorem" ?t "thm:" "~\\ref{%s}" t   ("theorem" "thm.") -3)
	("thm" ?t "thm:" "~\\ref{%s}" t   ("theorem" "thm.") -3)))
