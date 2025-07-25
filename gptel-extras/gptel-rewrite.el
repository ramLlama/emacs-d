;; taken from https://paste.karthinks.com/67b98a11-gptel-rewrite-with-context.html

;; An illustrative example of constructing a gptel directive for rewriting text
;; in a context-aware way.  A "gptel directive" is a canned system message and
;; conversation leading up to the action you want the LLM to perform.  See the
;; documentation of `gptel-directives' for more information on the format.

(defun gptel--rewrite-directive-buffer-aware ()
  "Directive for rewriting or refactoring that includes the current
buffer as context.

These are instructions not specific to any particular required
change.

The returned list (system message followed by canned exchanges)
is interpreted as the directive for the rewrite request.  To use
your own, add a different directive to `gptel-directives', which
see."
  (let* ((lang (downcase (gptel--strip-mode-suffix major-mode)))
         (article (if (and lang (not (string-empty-p lang))
                           (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                      "an" "a")))
    ;; We return a list containing the system message followed by a canned
    ;; series of alternating user and LLM response turns.
    (list
     ;; System message
     (format (concat "You are %s %s programmer.  "
                     "Follow my instructions and refactor %s code I provide.\n"
                     "- Generate ONLY %s code as output, without "
                     "any explanation or markdown code fences.\n"
                     "- Generate code in full, do not abbreviate or omit code.\n"
                     "- Do not ask for further clarification, and make "
                     "any assumptions you need to follow instructions.\n\n"
                     "For context, I will first provide you with code as it appears AFTER and BEFORE the text to be rewritten."
                     "  Do not repeat any of the AFTER or BEFORE code, only rewrite the text in the region")
             article lang lang lang)
     ;; User turn
     nil                                ;skipped
     ;; LLM turn
     "What is the code AFTER the cursor?"
     ;; User turn
     (format "AFTER\n```\n%s\n```\n"
              (buffer-substring-no-properties
               (if (use-region-p) (max (point) (region-end)) (point))
               (point-max)))
     ;; LLM turn
     "And what is the code BEFORE the cursor?"
     ;; User turn
     (format "BEFORE\n```%s\n%s\n```\n" lang
               (buffer-substring-no-properties
                (point-min)
                (if (use-region-p) (min (point) (region-beginning)) (point))))
     ;; LLM turn
     "What is the code to be refactored?")))

;; NOTE: ↑ We're not done.  There are two remaining conversation turns for fully
;; specifying the rewrite tsk, but these are handled in `gptel-rewrite' itself.
;; See `gptel--suffix-rewrite' for the last two.

;; Add this to the list of `gptel-directives'
(add-to-list 'gptel-directives
             `(rewrite-buffer-aware . ,#'gptel--rewrite-directive-buffer-aware))

;; 1. Now when you run M-x `gptel-rewrite', you can choose this directive from
;; the "Set full directive" menu.
;;
;; 2. Use the dry-run option to see exactly what will be sent.  Set
;; `gptel-expert-commands' to `t' if you don't see the dry-run options.
;;
;; 3. You can be as fancy as you'd like with supplying context, such as
;; including semantic relationships generated using tree-sitter and/or LSP that
;; might help the LLM do a better job.
