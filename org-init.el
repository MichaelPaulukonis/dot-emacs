;; my org-mode initialization
;; org-mode
;; initialize package prior to org
;;The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist' ("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; per http://orgmode.org/manual/Closing-items.html
(setq org-log-done 'time)
(setq org-log-done 'note)

(add-hook 'org-mode-hook 'turn-on-font-lock) ;org-mode buffers only

(setq org-agenda-files (list (concat dropbox-path "/Emacs/org/")))

(setq org-startup-folded "showeverything")

(global-set-key "\C-c\M-jw" 'org-journal-work)
(global-set-key "\C-c\M-jp" 'org-journal-personal)
(global-set-key "\C-c\M-jf" 'org-journal-freelance)
(defun org-journal-work ()
  "Send work-based directory to org-journal for day-job journaling."
  (interactive)
  (org-journal "D:/home/Personal/org-journal-work/" "work"))

(defun org-journal-personal ()
  "Send dropbox-based directory to org-journal for personal journaling."
  (interactive)
  (org-journal "D:/Dropbox/Emacs/org/org-journal-personal/" "personal"))


(defun org-journal-freelance ()
  "Send dropbox-based directory to org-journal for freelance journaling."
  (interactive)
  (org-journal "D:/Dropbox/Emacs/org/org-journal-freelance/" "freelance"))

(defun org-journal (&optional root suffix)
  "Open .org file named after today's date, format YYYY-MM-DD-Day.jnl,
in subdirectory named in variable root, set in ~/.emacs,
else as defined below.
"
  (interactive)
  (progn
    (setq root (or root "D:/Dropbox/Emacs/org-journal/"))
    (setq default-directory (year-month-dir root))
    (setq todays-date (format-time-string "%Y-%m-%d-%a" nil))
    (let ((sfx (if suffix (concat "." suffix) "")))
    (setq filename (concat todays-date sfx ".org")))

    ;; why prompt? just open the d***n thing
    ;; maybe universal-arg will cause a prompt....
    ;; (list (read-file-name
    ;;        "Open journal-org file: " default-directory filename nil filename))
    )

  (find-file filename) ;; switch to buffer if exists, or open file

  ;; following lines based on http://metajack.im/2009/01/01/journaling-with-emacs-orgmode/
  (widen)
  ;; heading is not working correctly if it is the result of (today)
  (let ((isearch-forward t) (heading (get-today)))
        (goto-char (point-min))
        (unless (org-goto-local-search-headings heading nil t)
          ((lambda ()
             (org-insert-heading)
             (insert heading)
             (insert "\n\n"))))
        (goto-char (point-max))
        ;; (org-show-entry)
        (widen)
        ;; (org-narrow-to-subtree)
        ;; (end-of-buffer)
        ;; (backward-char 2)
        ;; (unless (= (current-column) 2)
        ;;   (insert "\n\n  "))
))


;; region handling is a bit awkward
;; and doesn't look "lispy"
;; but it works
;; also brackets code with extra line-break at top and bottom
(defun org-insert-block (&optional lang)
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "language: ")
                       "")))
  (if (region-active-p)
      (kill-region (region-beginning) (region-end)))
  (insert (format "#+begin_src %s\n\n" lang))
  (if (region-active-p)
      (yank))
  (insert "\n\n#+end_src")
  (goto-char (- (point) 10) ;; magic-number = 1-char before "#+end_src"
  ))
