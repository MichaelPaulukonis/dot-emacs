;;; misc java-script functions
;;; Michael Paulukonis 2009


(defun js-make-bookmarklet (start end)
  "Takes the region and makes into into a one-line bookmarklet.
Removes comments and newlines, and replacing spaces with HTML entity."
  (interactive  "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    ;; REMOVE COMMENTS
    ;; NOTE: make sure to not include the xpath recursive descent operator
    (replace-regexp "[^\":]//.*$" "")
    (goto-char (point-min))
    ;; replace any-length whitespace with html-entity
    (replace-regexp "\\s-+" "%20")
    (goto-char (point-min))
    ;; remove line-breaks
    (replace-regexp "\n" "")))

(defun js-un-bookmarklet ()
  "Assumes the current-line is a bookmarklet, and expands it.
Doesn't handle loops conditions very well -- treats them as sep. lines. They work, but look ugly."
  (interactive)
  (save-restriction
    ;; narrow to current line
    (narrow-to-region (re-search-backward "^") (re-search-forward "$"))
    (goto-char (point-min))
    ;; remove spaces
    (replace-string "%20" " ")
    ;; remove line-breaks
    (goto-char (point-min))
    (replace-string ";" ";
")))