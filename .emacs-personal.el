; Dont show the GNU splash screen
(setq message-log-max t)
(setq inhibit-startup-message t)
(display-time)
(toggle-debug-on-error)
(setq-default scroll-step 1)               ; turn off jumpy scroll
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(show-paren-mode t)
(setq sentence-end-double-space nil) ;sentences end with a single space
(setq dired-recursive-copies t)
(setq visible-bell 1) ;; stop that infernal bell!


;; http://stackoverflow.com/questions/2091881/emacs-font-sizing-with-ctrl-key-and-mouse-scroll
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)


;; need to step up to v23....
(if (not (= 22 emacs-major-version))
    (global-visual-line-mode t))
;;(global-whitespace-mode t) ;;http://xahlee.org/emacs/whitespace-mode.html

; make whitespace-mode use just basic coloring
(setq whitespace-style (quote
  (spaces tabs newline space-mark tab-mark newline-mark)))

;;; http://www.emacswiki.org/emacs/DeletingWhitespace#toc3
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; http://trey-jackson.blogspot.com/2008/10/emacs-tip-26-cua-mode-specifically.html
(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only nil) ;; no transient mark mode (uhm, I LIKE t-m-m)
(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
(cua-mode)


;; http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/da14890ba363eee6#20af067d45c3905f
;;prohibits auto-copy of mouse-selection in graphical mode
(setq mouse-drag-copy-region nil)

(tool-bar-mode -1) ;; turn-off toolbar (-1 off, any pos value on)



(defun kill-start-of-line ()
"kill from point to start of line"
  (interactive)
  (kill-line 0)
)

;; move to column in current line
(defun repos (position)
  (interactive "nPosition: ")
  (move-to-column position)
)


;;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))



;; ;;TODO: whoah, something's wrong -- it's erasing the original W32 menu-item :-(
;; ;; rebind menu's print-buffer to w32-printing
;; ;; (define-key global-map
;; ;;   [menu-bar file print-buffer]
; ;;   '("Print Buffer" . w32-print-print-buffer-notepad))

;; ;; http://steve.yegge.googlepages.com/effective-emacs#item1
;; ;; Caps-Lock and Ctrl-key swapped


;; http://www.dotemacs.de/dotfiles/SteveMolitor.emacs.html
(defun push-line ()
  "Select current line, push onto kill ring."
  (interactive)
  (save-excursion
    (copy-region-as-kill (re-search-backward "^") (re-search-forward "$"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun back-window ()
  (interactive)
  (other-window -1)
)

;; navigation tweak -- C-right/left (arrow keys) now go to START of word, not the end
;; as default forward/backword word behavior is to move "over" the work
;; http://geosoft.no/development/emacs.html
(defun geosoft-forward-word ()
   ;; Move one word forward. Leave the pointer at start of word
   ;; instead of emacs default end of word. Treat _ as part of word
   (interactive)
   (forward-char 1)
   (backward-word 1)
   (forward-word 2)
   (backward-word 1)
   (backward-char 1)
   (cond ((looking-at "_") (forward-char 1) (geosoft-forward-word))
         (t (forward-char 1))))

(defun geosoft-backward-word ()
   ;; Move one word backward. Leave the pointer at start of word
   ;; Treat _ as part of word
   (interactive)
   (backward-word 1)
   (backward-char 1)
   (cond ((looking-at "_") (geosoft-backward-word))
         (t (forward-char 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Custom loading of site-lisp, etc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;(progn (cd "~/") (normal-top-level-add-subdirs-to-load-path))
;; ;;NOTE: this takes a while, and may be... a bit much....
;; ;; instead... but it throws errors??
;; ;; http://www.emacswiki.org/cgi-bin/wiki/LoadPath
;; ;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;; ;;     (let* ((my-lisp-dir "~/site-lisp/")
;; ;;      (default-directory my-lisp-dir))
;; ;;       (setq load-path (cons my-lisp-dir load-path))
;; ;;       (normal-top-level-add-subdirs-to-load-path)))


(defconst westbrook-laptop "MICHAELP-E6510" "westbrook win7 new laptop")
(defconst work-machine "MT5AI-3-3A" "computer-name at diversified")
(defconst netbook "MINIMALKEYS" "my netbook")
(defconst personal-laptop "CRYSTALLOGRAPH" "personal-latptop")
(defconst dropbox-path
  (cond ((string-equal (system-name) netbook) "c:/My Dropbox")
        ((string-equal (system-name) work-machine)  "e:/db/My Dropbox")
        ((string-equal (system-name) westbrook-laptop)  "d:/Dropbox")
        ((string-equal (system-name) personal-laptop)  "d:/Dropbox")
        (t "e:/apps/Dropbox"))
  "Path to DropBox ON THIS MACHINE")

(setq dropbox-emacs-path (concat dropbox-path "/Emacs/"))
(setq dropbox-site-lisp (concat dropbox-path "/Emacs/site-lisp/"))
(add-to-list 'load-path dropbox-site-lisp)
;; 2012.11.14 taking out as it loads before nxml mode for xml. and I hate that.
;; (add-to-list 'load-path (concat dropbox-site-lisp "psgml-1.3.2"))
;; (load "psgml.el")


(defun open-conkerorrc ()
  "open the conkerorrc dir"
  (interactive)
  (find-file (concat dropbox-path "/conkerorrc/")))



;; includes dropbox-parsing
(load "xrays-functions")

(defun edit-xrays ()
  "Load the xrays-functions.el file automatically."
  (interactive)
  (find-file (concat dropbox-site-lisp "xrays-functions.el"))
)



;;;(setq dropbox-site-lisp "c:/Documents and Settings/OtherMichael/My Documents/My Dropbox/Emacs/site-lisp/")
;;(setq dropbox-site-lisp "c:/My Dropbox/Emacs/site-lisp/")
;;(add-to-list 'load-path dropbox-site-lisp)


;;(add-to-list 'load-path "~/site-lisp/")



;; as of 2013.05.16 I HAVE NOT USED THIS IN YEARS!!!
;; ;; pmwiki-mode
;; ;; http://www.pmwiki.org/wiki/Cookbook/EmacsPmWikiMode
;; ;; see also http://www.emacswiki.org/cgi-bin/wiki/WikiModeDiscussion
;; (add-to-list 'load-path "~/site-lisp/pmwiki-mode")
;; (require 'pmwiki-mode)

;; (setq pmwiki-main-wiki-base-uri "http://www.xradiograph.com/")
;; (setq pmwiki-main-homepage-uri
;;       (concat pmwiki-main-wiki-base-uri "XraysMonaLisa.XraysMonaLisa"))

;; (set 'pmwiki-author "OtherMichael")

;;keybinding, below
;; liberal use of dv-clipboard-copy-backslashified from http://www.emacswiki.org/emacs/JonathanArnoldDotEmacs
(defun tell-me-the-name ()
  "If buffer is visiting file or dir, echos FULL PATH (name) to mini-buffer, and places in kill-ring,
  replacing slashes with backslashes.
  If buffer is not visiting file or dir, does this to the buffer-name."
  (interactive)
  (save-excursion
    (let ((name (if (string-equal mode-name "Dired by name") dired-directory
                  (buffer-file-name)))
          (temp-buf (get-buffer-create " dv-temp-clip-buf")))
      (if (not name) (set 'name (buffer-name)))
      (set-buffer temp-buf)
      (if name (insert name))
      (goto-char (point-min))
      (replace-string "/" "\\")
      (clipboard-kill-ring-save (point-min) (point-max))
      (kill-buffer temp-buf)
      (message (current-kill 0)))))


(defun wiki-edit-config ()
  (interactive)
  (find-file "D:/projects/www/1and1/projects/xradiograph.com/wiki/local/config.php"))

;; ;from http://www.hakank.org/emacs/index.html
;; ;; this should be defined in the main .emacs
;; because the home dir doesn't hold this file
;; (defun edit-emacs ()
;;   "Load the .emacs file automatically."
;;   (interactive)
;;   (find-file "~/.emacs-personal.el")
;; )

;; http://stackoverflow.com/questions/437714/how-to-do-use-c-x-k-to-kill-an-emacs-buffer-opened-in-server-mode
(add-hook 'server-switch-hook
  (lambda ()
    (local-set-key (kbd "C-x k") '(lambda ()
                                    (interactive)
                                    (if server-buffer-clients
                                        (server-edit)
                                      (ido-kill-buffer))))))



;; php-mode
;; http://php-mode.sourceforge.net/
(add-to-list 'load-path (concat dropbox-site-lisp "php-mode-1.4.0/"))
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; my macros

(column-number-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(load "skipscreen-functions")


(load "extraedit.el")

(defun edit-text ()
  "Load the extraedit.el file automaticall."
  (interactive)
  (find-file (concat dropbox-site-lisp "extraedit.el"))
)


(load "csv-mode.el")

;; string-functions
;; http://www.splode.com/~friedman/software/emacs-lisp/src/string-fns.el
(load-file (concat dropbox-site-lisp "string-fns.el"))


(defun force-underscore ()
  "Replace spaces with underscores in the current line.
Common for file-names, etc."
  (interactive)
  (save-excursion
    (narrow-to-region (re-search-backward "^") (re-search-forward "$"))
    (beginning-of-line)
    (replace-string " " "_")
    (widen)
    )
  )



;; work with Personal Journal
;; http://www.emacswiki.org/cgi-bin/emacs-en/Journal
;; ~/site-lisp/journal.el
;; load calendar
;; from http://www.hakank.org/emacs/index.html
;; for editing the journal code,
(defun edit-journal ()
  "Load the journal.el file automatically."
  (interactive)
  (find-file (concat dropbox-site-lisp "journal.el"))
)

;; TODO: create buffer-local variables,
;; so that each file can belong to a different root-set
;; of journals -- project oriented, f'r instance
;;
;; I _thought_ I had done this.... but it doesn't work....
(load "journal")
(setq journal-dir "~/Personal/Journal/")
(add-to-list 'auto-mode-alist '("\\.jnl\\'" . journal-mode))

;; TODO: make a work-journal and a personal-journal
;; the personal-journal should be shared on Dropbox...
(define-key global-map "\C-xj" '(lambda () (interactive) (journal "~/Personal/Journal/")))
(define-key global-map "\C-xg" '(lambda () (interactive) (journal (concat dropbox-path "/grad-school/journal/"))))

(defun job-journal ()
  "Open a job-hunt journal entry"
  (interactive)
  (journal (concat dropbox-path "/job-hunt/journal/")))

;;; okay, so now I don't need to load these anymore....
;;; some interesting code in there, though
;;;
;;; (load "dit-functions")


;;;;  execute to initialize elpa on a new install
;;;; TODO: I suppose it is possible to have this auto-execute if it finds it is not installed....
 ;; (let ((buffer (url-retrieve-synchronously
 ;;               "http://tromey.com/elpa/package-install.el")))
 ;;  (save-excursion
 ;;    (set-buffer buffer)
 ;;    (goto-char (point-min))
 ;;    (re-search-forward "^$" nil 'move)
 ;;    (eval-region (point) (point-max))
 ;;    (kill-buffer (current-buffer))))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;;;(when
;;;    (load
;;;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;;;  (package-initialize))

;; this does not work with Emacs 23
;; TODO: upgrade to emacs 24 ... AND MAKE SOME DAMN NOTES ABOUT IT
(require 'package)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; errors for marmalade on 2013.05.16
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(package-initialize)

;; load org-mode init-file
(load (concat dropbox-emacs-path "org-init.el"))

;; http://www.emacswiki.org/cgi-bin/wiki/DiredPlus
(load "dired+") ;; stored in ~/emacs/lisp
(load "w32-browser") ;; open file on current line (etc.)
(define-key dired-mode-map (kbd "C-RET") 'dired-w32-browse)


;; stop that OTHER mode from loading....
(add-to-list 'auto-mode-alist '("\\.html\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.fht\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . nxml-mode))

;; ;; http://www.math.umd.edu/~halbert/dotemacs.html
;; ;; NOT WORKING
;; ;;
;; ;; redefining the make-backup-file-name function in order to get
;; ;; backup files in ~/.backups/ rather than scattered around all over
;; ;; the filesystem. Note that you must have a directory ~/.backups/
;; ;; made.  This function looks first to see if that folder exists.  If
;; ;; it does not the standard backup copy is made.
;; ;; (defun make-backup-file-name (file-name)
;; ;;   "Create the non-numeric backup file name for `file-name'."
;; ;;   (require 'dired)
;; ;;   (if (file-exists-p "~/.backups")
;; ;;       (concat (expand-file-name "~/.backups/")
;; ;;               (replace-regexp-in-string "/" "!" file-name))
;; ;;     (concat file-name "~")))

;;; http://www.skrakes.com/?p=146
;; create a backup file directory
;; (defun make-backup-file-name (file)
;;   (concat "~/.backups/" (file-name-nondirectory file) "~"))

(setq backup-directory-alist '(("." . "~/.backups")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir "~/autosaves/")

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir "~/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))



;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
;; required for xsteve-ido-choose, below...
(autoload 'ido "ido")
(ido-mode t)
;; http://www.nabble.com/Added-UNC-support-to-ido.el-find-file.-td3467317.html
;;(setq ido-unc-hosts '("PaScrIcf1" "WpDit1" "PaScri4" "WpCigna1"))

;; http://xsteve.at/prg/emacs/power-user-tips.html
;; recentf
(autoload 'recentf "recentf")
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
;; keybinding, below


(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

;;; http://www.emacswiki.org/emacs/DeskTop
;;; updated on 2012.12
;;; needs to have a hook on close...
;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
;; (setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")


;; remove desktop after it's been read
;; WHY would we want to do this ?????
;; charmingly, this interacted with "save if exists"
;; in the manner you'd expect AFTER YOU THOUGHT ABOUT IT
;; in other words, since THIS function deleted it
;; it could never be saved again SINCE IT NO LONGER EXISTS
;; (add-hook 'desktop-after-read-hook
;; 	  '(lambda ()
;; 	     ;; desktop-remove clears desktop-dirname
;; 	     (setq desktop-dirname-tmp desktop-dirname)
;; 	     (desktop-remove)
;; 	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
;; (add-hook 'after-init-hook
;; 	  '(lambda ()
;; 	     (if (saved-session)
;; 		 (if (y-or-n-p "Restore desktop? ")
;; 		     (session-restore)))))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
;; Tell emacs to save backups in the global backups directory...
(defun make-backup-file-name(file)
  (concat "~/backups/" (file-name-nondirectory file) "~"))

;; http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html
(autoload 'uniquify "uniquify")
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; ;; this is left in as an example....

;; ;; I make no apologies for my maths being awful, so here's my get
;; ;; out of jail free function.  Sometimes it's nice instead of going
;; ;; to *scratch* and evaluating some stuff.
;; ;; It's also dead useful for mpuz, too...
;; (defun mult-table()
;;   "Prints a formatted multiplication table."
;;   (interactive)
;;   (switch-to-buffer "*MULT*")
;;   (erase-buffer)
;;   (insert "Multiplication table:\n\n")
;;   (let ((i 1))
;;                                      ; Start of table.  Print header.
;;     (insert "     1  2  3  4  5  6  7  8  9 10\n")
;;     (while (< i 11)
;;       (setq j 1)
;;       (if (< i 10)
;;      (insert (format "\n %d " i))
;;      (insert (format "\n%d " i))
;;      )
;;       (while (< j 11)
;;      (setq k (* i j))
;;      (if (< k 10)
;;        (insert (format "  %d" k))
;;        (insert (format " %d" k))
;;        )
;;      (setq j (+ j 1))
;;      )
;;       (setq i (+ i 1))
;;       )
;;     )
;;   (beginning-of-buffer)
;;   )

;; (setq ascii-unprint-chars-low ["NUL " "SOH " "STX " "ETX " "EOT "
;;                             "ENQ " "ACK " "BEL " "BS  " "HT  "
;;                             "LF  " "VT  " "FF  " "CR  " "SO  "
;;                             "SI  " "DLE " "DC1 " "DC2 " "DC3 "
;;                             "DC4 " "NAK " "SYN " "ETB " "CAN "
;;                             "EM  " "SUB " "ESC " "FS  " "GS  "
;;                             "RS  " "US  "])


;; (defun ascii-table ()
;;   "Prints a formatted ASCII table.  With control characters symbolically shown"
;;   (interactive)
;;   (switch-to-buffer "*ASCII*")
;;   (erase-buffer)
;;   (insert "ASCII Table:\n\n")
;;   (let ((i 0))
;;     (let ((j 0))
;;       ; Start of table.  Print header.
;;       (insert "    0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F")
;;       (while (< i 16)
;;      (setq j 0)
;;      ; Add in "Not Ascii after this point seperator" if i = 8
;;      (if (= i 8)
;;          (insert "\n\nCharacters after 127 aren't defined in the ASCII spec\n but are defined on this computer's locale as\n")
;;        )
;;      ; start of new line, insert table index
;;      (insert (format "\n %X  " i))
;;      (while (< j 16)
;;        (let ((char-num (+ (* i 16) j)))
;;          (if (or (< char-num 32))
;;              (insert (aref ascii-unprint-chars-low char-num))
;;            (if (= char-num 127)
;;                (insert "DEL ")
;;              (if (or (< char-num 127) (> char-num 159))
;;                  (insert (format "%c   " char-num))
;;                (insert "    ")
;;                )
;;              )
;;            )
;;          )
;;        (setq j (+ j 1))
;;        )
;;      (setq i (+ i 1))
;;      )
;;       )
;;     )
;;   (beginning-of-buffer)
;;   )

;; TODO: this should probably, you know, be elsewhere...
;; set default directory
(cd "~")


;; http://steve.yegge.googlepages.com/my-dot-emacs-file
;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
         (w2 (second (window-list)))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))


(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
        (filename (buffer-file-name)))
 (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
         (message "A buffer named '%s' already exists!" new-name)
        (progn           (rename-file name new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil))))))


(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
         (if (string-match dir "\\(?:/\\|\\\\)$")
         (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
   (if (not filename)
     (message "Buffer '%s' is not visiting a file!" name)
   (progn       (copy-file filename newname 1)
                (delete-file filename)
                (set-visited-file-name newname)
                (set-buffer-modified-p nil)     t))))

;; http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jao-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(defun selective-display-column ()
  "set selective display fold everything greater than the current column, or toggle off if active"
  (interactive)
  (set-selective-display
   (if selective-display nil (or (+ (current-column) 1) 1))))


;;; portions from http://www.emacswiki.org/emacs/ColdFusionDeluxeMode
;; (load "coldfusion.el")
;; (add-to-list 'auto-mode-alist '("\\.cfm" . sgml-html-mode))


;; common-lisp/slime enviro
;; (defun run-clisp ()
;;   (interactive)
;;   (progn
;;     (load "lispbox")
;;     (slime)
;;     )
;;   )

;; http://www.khngai.com/emacs/perl.php
;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))


;; ;; run the current perl program
;; ;; NOTE: shell buffer has some io "buffering" issues and is not recco'd as of Nov 26, 2007 by mjp
;; ;; http://www.dotfiles.com/files/6/108_.emacs
;; (defun run-perl ()
;;   (interactive "*")
;;   (setq perl-buffer-name buffer-file-name)
;;   (shell)
;;   (setq perl-run-command "perl \"")
;;   (insert perl-run-command)
;;   (insert perl-buffer-name)
;;   (insert "\"")
;; )

(put 'set-goal-column 'disabled nil)



;;; http://benjisimon.blogspot.com/2010/08/handy-emacs-function-url-decode-region.html
(load "url-util")

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))


;; I'd rather use nxml.....
;;(autoload 'nxhtml "nxhtml" "major mode")


;;; e-text reader
(autoload 'etxt "etext")


;;; this presumes that the correct path is already in the environment-variable PATH
(setq-default ispell-program-name "aspell.exe")
(setq text-mode-hook '(lambda()
			(flyspell-mode t)       ; spellchek (sic) on the fly
			))

;;; PROGRAMMING MODES

;; TODO: some perl things, above

(autoload 'asp-mode "asp-mode")
(setq auto-mode-alist
      (cons '("\\.asp\\'" . asp-mode) auto-mode-alist))



;; WIN-DOS BATCH MODE
;; http://ftp.azc.uam.mx/mirrors/gnu/windows/emacs/docs/ntemacs/contrib/bat-mode.el
(setq auto-mode-alist
      (append
       (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
       ;; For DOS init files
       (list (cons "CONFIG\\."   'bat-mode))
       (list (cons "AUTOEXEC\\." 'bat-mode))
       auto-mode-alist))

(autoload 'bat-mode "bat-mode"
  "DOS and WIndows BAT files" t)

;; allout-mode
;; http://www.emacswiki.org/cgi-bin/wiki/AllOut
;; http://myriadicity.net/Sundry/EmacsAllout
(require 'allout)
(allout-init t)

(defvar rf-allout-font-lock-keywords
  '(;;
    ;; Highlight headings according to the level.
    (eval . (list (concat "^\\(" outline-regexp "\\).+")
                0 '(or (cdr (assq (outline-depth)
                                  '((1 . font-lock-function-name-face)
                                    (2 . font-lock-variable-name-face)
                                    (3 . font-lock-keyword-face)
                                    (4 . font-lock-builtin-face)
                                    (5 . font-lock-comment-face)
                                    (6 . font-lock-constant-face)
                                    (7 . font-lock-type-face)
                                    (8 . font-lock-string-face))))
                       font-lock-warning-face)
                nil t)))
  "Additional expressions to highlight in Outline mode.")

;; add font-lock to allout mode
(defun rf-allout-font-lock-hook ()
  (set (make-local-variable 'font-lock-defaults)
       '(rf-allout-font-lock-keywords t
nil nil outline-back-to-current-heading)))

(add-hook 'outline-mode-hook 'rf-allout-font-lock-hook)

;;;c:/Documents and Settings/OtherMichael/My Documents/My Dropbox/Emacs/site-lisp/company/
(add-to-list 'load-path (concat dropbox-site-lisp "company"))
(autoload 'company-mode "company" nil t)


(autoload 'camelCase-mode "camelCase-mode" nil t)
(add-hook 'java-mode-hook '(lambda () (camelCase-mode 1)))
(add-hook 'js2-mode-hook '(lambda () (camelCase-mode 1)))
(add-hook 'javascript-mode-hook '(lambda () (camelCase-mode 1)))
(add-hook 'cperl-mode-hook '(lambda () (camelCase-mode 1)))

;; cygwin
;; http://www.khngai.com/emacs/cygwin.php
;; after Perl, but before windows... hrm....
;; 05/24/10 9:11 AM - AFTER everything.....
;;; if Perl is not installed, this is particularly stupid.....
;; (setenv "PATH" (concat  (getenv "PATH") "C:\\Perl\\site\\bin;C:\\Perl\\bin;c:\\bin;"))
;; (add-to-list 'exec-path
;;   (cond ((string-equal (system-name) netbook) "c:\\cygwin\\bin")
;;         ((string-equal (system-name) work-machine)  "c:\\bin")
;; 	((string-equal (system-name) westbrook-laptop) "d:\\cygwin\\bin")
;;         (t "c:\\cygwin\\bin"))
;;   t)
;; GnuWin32 tools should be before cygwin? or swap, or something? aaaargh
;; required for ediff, etc
;; below path is static for work-machine
;;(setenv "PATH" (concat  "T:/Emacs/EmacsW32/gnuwin32/bin" (getenv "PATH")))

;; (add-to-list 'exec-path "c:/cygwin/bin")
;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

;; if bash shell already open, prompt for new creation
;; becuase sometimes I want a new buffer; sometimes I just want to find the existing....
;; (defun bash ()
;;   (interactive)
;;   (if (cygwin-shell)
;;       (rename-buffer "bash" 1)))


;;; http://www.khngai.com/emacs/cygwin.php
(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (setq comint-input-sender 'n-shell-simple-send)
  )

;;; man-command removed
(defun n-shell-simple-send (proc command)
  "17Jan02 - sailor. Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer)
    )
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))
   )
  )



;; ;; http://peadrop.com/blog/2007/05/11/blogging-with-emacs/
;; ;; http://www.emacswiki.org/cgi-bin/wiki/WebloggerMode
;; ;; BUG: http://www.tolchz.net/2008/01/06/posting-to-wordpress-with-emacs-webloggerel/
;; ;;
;; ;; installed Feb 11, 2008 via ELPA
;; (add-to-list 'load-path "~/.emacs.d/weblogger")
;; (require 'weblogger)
;; (put 'narrow-to-region 'disabled nil)


;;; well this is bound to not work on most of my current machines. if any..... 2013.04.10
(defun firefox-profiles ()
  (interactive)
  (dired "C:/Documents and Settings/OtherMichael/Application Data/Mozilla/Firefox/Profiles"))





;; http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
;; see also http://stackoverflow.com/questions/889649/run-sgml-pretty-print-when-opening-an-xml-file-in-emacs
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(add-hook 'find-file-hook 'my-nxml-find-file-hook)
(defun my-nxml-find-file-hook ()
  "run sgml pretty-print on the file when it's opened (if it's sgml)"
  (when (eq major-mode 'xml-mode)
    (bf-pretty-print-xml-region (point-min) (point-max))))


(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)

(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line

      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))


;;;http://emacswiki.org/emacs/AlignCommands#toc5
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

;; ;; w3m browser
;; ;; June 17, 2007
;; ;; http://www.emacswiki.org/cgi-bin/wiki/emacs-w3m
;; ;; http://w3m.sourceforge.net/MANUAL
;; ;; http://bc.tech.coop/blog/080110.html
;; (add-to-list 'load-path "~/site-lisp/w3m")
;; (setq w3m-command (concat "c:/Cygwin/bin/w3m.exe" ""))
;; (require 'w3m-load)

;; (require 'w3m-search)
;; (add-to-list 'w3m-search-engine-alist
;;           '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s"))

;; (eval-after-load "dired"
;;   '(define-key dired-mode-map "\C-xm" 'dired-w3m-find-file))

;; (defun dired-w3m-find-file ()
;;   (interactive)
;;   (require 'w3m)
;;   (let ((file (dired-get-filename)))
;;     (if (y-or-n-p (format "Use emacs-w3m to browse %s? "
;;                        (file-name-nondirectory file)))
;;      (w3m-find-file file))))

;; (require 'w3m-lnum)
;; (defun jao-w3m-go-to-linknum ()
;;   "Turn on link numbers and ask for one to go to."
;;   (interactive)
;;   (let ((active w3m-link-numbering-mode))
;;     (when (not active) (w3m-link-numbering-mode))
;;     (unwind-protect
;;      (w3m-move-numbered-anchor (read-number "Anchor number: "))
;;       (when (not active) (w3m-link-numbering-mode)))))

;; (define-key w3m-mode-map "f" 'jao-w3m-go-to-linknum)


;; twitter
;; http://www.emacswiki.org/cgi-bin/wiki/TwIt
;; see also http://tapsellferrier.hapispace.com/nicferrier/200710115T225511.hapi
;;(load-file "~/emacs/lisp/xml.el") ;; becuase (require 'xml) is defaulting to the sgml code, above
;;(load-file "~/site-lisp/twit.el")



;; ;; g-client
;; ;; google-clients
;; ;; http://emacspeak.blogspot.com/2007/03/emacs-client-for-google-services.html
;; ;; http://groups.google.com/group/emacs-g-client
;; ;; http://emacspeak.blogspot.com/2007/07/emacs-g-client-reader-and-cse-searching.html
;; ;; June, 2008
;; ;; not, uhm, successful?
;; (add-to-list 'load-path "~/site-lisp/g-client")
;; (load-library "g")
;; (setq g-html-handler 'w3m)




;; figlet fonts
;; stored in ~/figlet-fonts
;; runs the cygwin figlet program (see Cygwin customizations, above)
;;;(require 'figlet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROGRAMMING MODES + SETTINGS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JavaScript
;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(load "js-funcs")
(load "greasemonkey-fns")

;; Steve Yegge's JS2 mode
;; http://code.google.com/p/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-highlight-vars "js2-highlight-vars")

;;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(load "espresso")
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))


(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (autoload 'espresso "espresso")
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 8)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)]
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  ;; fix bug with my-indent-sexp
  (setq c-current-comment-prefix
	(if (listp c-comment-prefix-regexp)
	    (cdr-safe (or (assoc major-mode c-comment-prefix-regexp)
			  (assoc 'other c-comment-prefix-regexp)))
	  c-comment-prefix-regexp))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(setq-default indent-tabs-mode nil)

(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq tab-width 2)
(add-hook 'c-mode-common-hook
  #'(lambda ()
    (setq c-basic-offset tab-width)))

;; based on notes @ http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
(require 'auto-complete-config)
(setq ac-ignore-case nil)
(setq ac-auto-start 2)
(global-auto-complete-mode t)

;; auto-call jslint via M-x compile
;; http://www.corybennett.org/projects/
(add-hook 'javascript-mode-hook
  (lambda ()
    ;;; make emacs recognize the error format produced by jslint
    (set (make-local-variable 'compilation-error-regexp-alist)
       '(("^\\([a-zA-Z.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
    (set (make-local-variable 'compile-command)
       (let ((file (file-name-nondirectory buffer-file-name)))
;;          (concat "perl c:/cygwin/bin/jslint --laxbreak --browser --undef " file)))))
          (concat "perl c:/cygwin/bin/jslint " file)))))


;;; http://stackoverflow.com/a/11271313/41153
(require 'js-comint)
(setq inferior-js-program-command "node  --interactive")

;;; http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))


;; for resets
;;(set 'javascript-mode-hook nil)

;;; superceded by JS2-mode
;;; http://www.emacswiki.org/emacs/JavaScriptMode
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;;(autoload 'javascript-mode "javascript" nil t)


;; ;;; MozRepl
;; ;;; http://wiki.github.com/bard/mozrepl/emacs-integration
;; ;; well, it's not working for me...
;; ;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; ;; (add-hook 'javascript-mode-hook 'java-custom-setup)
;; ;; (defun javascript-custom-setup ()
;; ;;   (moz-minor-mode 1))


;; http://wiki.github.com/jrockway/eproject/installingeproject
;; (add-to-list 'load-path (concat dropbox-site-lisp "eproject"))
;; (require 'eproject)
;; (require 'eproject-extras)

;; ;; eproject global bindings
;; (defmacro .emacs-curry (function &rest args)
;;   `(lambda () (interactive)
;;      (,function ,@args)))

;; (defmacro .emacs-eproject-key (key command)
;;   (cons 'progn
;;          (loop for (k . p) in (list (cons key 4) (cons (upcase key) 1))
;;                collect
;;                `(global-set-key
;;                  (kbd ,(format "C-x p %s" k))
;;                  (.emacs-curry ,command ,p)))))
;; (.emacs-eproject-key "k" eproject-kill-project-buffers)
;; (.emacs-eproject-key "v" eproject-revisit-project)
;; (.emacs-eproject-key "b" eproject-ibuffer)
;; (.emacs-eproject-key "o" eproject-open-all-project-files)
;; ;;; Key sequence C-x p f starts with non-prefix key C-x p
;; ;;;(global-set-key (kbd "C-x p f") 'eproject-find-file)

;; (define-project-type ff-extension (generic)
;;   (look-for "install.rdf")
;;   :relevant-files ("\\.js$" "\\.xul$" "\\.rdf$" "\\.sh$" "\\.pl$"))


;; http://nflath.com/2009/07/imenu/
;; see keybindings
(autoload 'imenu "imenu")
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (if (markerp position)
          (goto-char position) (goto-char (overlay-start position))))))


;;;;;;;;;;;
;;; CEDET

;; commented-out on August 14 due to severe slow-downs at launch
;; client-server was not starting :-(

;; ;; Load CEDET
;; (load-file "~/site-lisp/cedet-1.0pre4/common/cedet.el")
;; (add-to-list 'load-path "~/site-lisp/cedet-1.0pre4/speedbar/")
;; (load-file "~/site-lisp/cedet-1.0pre4/speedbar/speedbar.el")

;; ;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; ;; Select one of the following:

;; ;; * This enables the database and idle reparse engines
;; ;;(semantic-load-enable-minimum-features)

;; ;; * This enables some tools useful for coding, such as summary mode
;; ;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)

;; ;; * This enables even more coding tools such as the nascent intellisense mode
;; ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; ;; (semantic-load-enable-guady-code-helpers)

;; ;; * This turns on which-func support (Plus all other code helpers)
;; ;; (semantic-load-enable-excessive-code-helpers)

;; ;; This turns on modes that aid in grammar writing and semantic tool
;; ;; development.  It does not enable any other features such as code
;; ;; helpers above.
;; ;; (semantic-load-enable-semantic-debugging-helpers

;; http://blog.tuxicity.se/elisp/emacs/2010/04/26/comment-stuff-in-in-emacs.html
(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (and transient-mark-mode mark-active)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))




;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAVIGATION
;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; suggested by http://osdir.com/ml/emacs.sources/2005-05/msg00003.html
(defun find-in-open (regex)
  (interactive "sRegex search: ")
  ;; for whateve reason, Gomoku screws it up
  (multi-occur (remove (get-buffer "*Gomoku*") (buffer-list)) regex))
;; http://www.emacswiki.org/emacs/OccurMode
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)

(load "custom-occur") ;; C-S o - occur in this buffer; other bindings in file

;; TODO: there's another function doing this, above....
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; iBuffer
;; default groups for ibuffer
;; http://www.shellarchive.co.uk/content/emacs_tips.html#sec17
;; http://www.emacswiki.org/emacs/IbufferMode#toc5
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("webtext" (filename . "/WebText/"))
               ("javascript" (or
                        (mode . javascript-mode)
                        (filename . "/greasemonkey/")
                        (filename . "/javascript/")
                        (name . "greasemonkey-fns.el")))
              ("xrad" (or
                        (name . ".*xradiograph\\.com.*")
                        (name . "XraysMonaLisa")
                        (filename . "c:/www/xradiograph/")))
               ("firefox" (filename . "/mozilla/firefox/"))
               ("job-hunt" (or
                            (filename . "/job-hunt/")
                            (filename . "resume")))
               ("dropbox" (filename . "dropbox"))
               ("perl" (mode . cperl-mode))
               ("journal" (mode . journal-mode))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "diary")))
               ("w3m"  (name . ".*w3m.*"))
               ("emacs" (or
                         (mode . help-mode)
                         (mode . occur-mode)
                         (mode . Info-mode)
                         (mode . bookmark-bmenu-mode)
                         (mode . emacs-lisp-mode)
                         (name . "^\\*Apropos\\*$")
                         (name . "^.emacs$")
                         (name . "el$")
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (filename . "/site-lisp/")))
               ("dired" (mode . dired-mode))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))))))

;; ibuffer, I like my buffers to be grouped
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups
             "default")))

;;; http://www.emacswiki.org/emacs/ImenuMode
;; (defun ido-goto-symbol ()
;;   "Will update the imenu index and then use ido to select a symbol to navigate to"
;;   (interactive)
;;   (imenu--make-index-alist)
;;   (let ((name-and-pos '())
;;         (symbol-names '()))
;;     (flet ((addsymbols (symbol-list)
;;                        (when (listp symbol-list)
;;                          (dolist (symbol symbol-list)
;;                            (let ((name nil) (position nil))
;;                              (cond
;;                               ((and (listp symbol) (imenu--subalist-p symbol))
;;                                (addsymbols symbol))

;;                               ((listp symbol)
;;                                (setq name (car symbol))
;;                                (setq position (cdr symbol)))

;;                               ((stringp symbol)
;;                                (setq name symbol)
;;                                (setq position (get-text-property 1 'org-imenu-marker symbol))))

;;                              (unless (or (null position) (null name))
;;                                (add-to-list 'symbol-names name)
;;                                (add-to-list 'name-and-pos (cons name position))))))))
;;       (addsymbols imenu--index-alist))
;;     (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
;;            (position (cdr (assoc selected-symbol name-and-pos))))
;;       (goto-char position))))


;;; breadcrumb
(autoload 'breadcrumb "breadcrumb")
(global-set-key [(f9)]         'bc-set)
(global-set-key [(shift f9)]   'bc-list)
(global-set-key [(control f9)] 'bc-previous)
(global-set-key [(meta f9)]    'bc-next)
(global-set-key [(shift control f9)] 'bc-local-previous)
(global-set-key [(shift meta f9)]    'bc-local-next)
(global-set-key [(control c)(j)]     'bc-goto-current) ;; C-c j for jump to current breadcrumb



;;http://www.xsteve.at/prg/emacs/.emacs.txt
;; trying to get a simple function to act on passed string.... keep working.....
(defun xsteve-exchange-slash-and-backslash ()
  "Exchanges / with \ and in the current line or in the region when a region-mark is active."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((replace-count 0)
            (eol-pos (if mark-active (region-end) (progn (end-of-line) (point))))
            (bol-pos (if mark-active (region-beginning) (progn (beginning-of-line) (point)))))
        (goto-char bol-pos)
        (while (re-search-forward "/\\|\\\\" eol-pos t)
          (setq replace-count (+ replace-count 1))
          (cond ((string-equal (match-string 0) "/") (replace-match "\\\\" nil nil))
                ((string-equal (match-string 0) "\\") (replace-match "/" nil nil)))
          (message (format "%d changes made." replace-count)))))))


;;; auto-byte compile .emacs
;;; http://stackoverflow.com/questions/778716/how-can-i-make-emacs-start-up-faster
;;; added 2012.12.19
(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "d:/Dropbox/Emacs/.emacs-personal.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
      (byte-compile-file dotemacs))))


(add-hook 'after-save-hook 'autocompile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; keybindings (unless part of a large grouping)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map [?\C-']     'other-window) ;;http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks near bottom
(define-key global-map [?\C-\"]    'back-window) ;; can't find a way to bind w/ arguement
(global-set-key [?\M-\"]           'swap-windows)

;; note binding tips at http://tiny-tools.sourceforge.net/emacs-keys-body.html#case_study_why_cant_i
(define-key global-map "\M-#"      'kill-start-of-line)
(define-key global-map "\C-xw"     'fixup-whitespace)
(define-key global-map "\C-x\C-g"  'repos) ;; reposition (ie - goto-line)
(define-key global-map "\C-x _"    'force-underscore)

(define-key global-map "\C-xnt"    'today) ;; from journal.el

;; [f1]                            'jao-toggle-selective-display (defined below)
(define-key global-map [f1]        'delete-other-windows)
(define-key global-map [f2]        'selective-display-column)
(define-key global-map [M-f2]      'jao-toggle-selective-display)
(define-key global-map [f3]        'save-buffer) ;; http://slashdot.org/comments.pl?sid=162694&cid=13599510
(define-key global-map [f4]        'kill-buffer)

(define-key global-map [f5]        'toggle-truncate-lines)
(define-key global-map [f6]        'other-window)
(define-key global-map [f7]        'find-in-open)
(global-set-key (kbd "<f8> t")     'planner-create-task-from-buffer)

(define-key global-map [f10]       'switch-to-previous-buffer)
(define-key global-map [f11]       'ibuffer) ;; 'h' after entering for help
(define-key global-map [f12]       'browse-url-at-point)
(global-set-key [(meta f12)]       'recentf-open-files)

(global-set-key (kbd "C-7")        'comment-or-uncomment-current-line-or-region)

(global-set-key [S-mouse-3]        'imenu) ;;  (info "(Emacs)Imenu")

(define-key global-map "\C-xt"     'tell-me-the-name) ;; old binding C-xnb is used by org-mode

(global-set-key (kbd "M-/")        'hippie-expand)  ;; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(global-set-key "\C-xy"            'push-line)

(global-set-key [C-right]          'geosoft-forward-word)
(global-set-key [C-left]           'geosoft-backward-word)

(global-set-key (kbd "M-+")        'text-scale-adjust)
(global-set-key (kbd "M--")        'text-scale-adjust) ;; OVER-RIDES previous 'negative-argument (also on C-M--, M--)
(global-set-key (kbd "M-0")        'text-scale-adjust)


(global-set-key (kbd "M-|")        'xsteve-exchange-slash-and-backslash)

;; C-c q toggles auto-fill, also known as word wrap
(global-set-key (kbd "C-c q")      'auto-fill-mode)

(define-key global-map "\C-xj"     'journal)
(define-key global-map "\C-xg"     '(lambda () (interactive) (journal "/grad-school/journal/")))

(global-set-key (kbd "M-<return>") 'lisp-complete-symbol)
(global-set-key (kbd "M-s")        'ido-goto-symbol) ;; completion list
(global-set-key (kbd "M-S")        'imenu) ;; defaults to current symbol

(global-set-key (kbd "C-M-|")      'indent-rigidly) ;; add to exisitng indentation

(global-set-key (kbd "C-c r")      'comment-region)

;; simple aliases (too short, but will do for now)
(defalias 'qrr 'query-replace-regexp)
(defalias 'dml 'delete-matching-lines)
(defalias 'kml 'delete-non-matching-lines)
(defalias 'eli 'elisp-index-search)
(defalias 'vbm 'visual-basic-mode)
;;(defalias 'bash 'cygwin-shell)
(defalias 'swap-slashes 'xsteve-exchange-slash-and-backslash)
(defalias 'slash-swap 'xsteve-exchange-slash-and-backslash)


;; as seen at http://www.emacswiki.org/emacs-en/EmacsNiftyTricks#toc4 (buffers and files)
;; C-x r j <+ register letter>
(set-register ?e (cons 'file (concat dropbox-path "/Emacs/.emacs-personal.el")))
(set-register ?d (cons 'file (concat dropbox-site-lisp "dit-functions.el")))
(set-register ?g (cons 'file (concat dropbox-site-lisp "greasemonkey-fns.el")))
(set-register ?j (cons 'file (concat dropbox-site-lisp "journal.el")))
(set-register ?x (cons 'file (concat dropbox-site-lisp "xrays-functions.el")))
(set-register ?w (cons 'file (concat dropbox-path "/projects/WebText/")))
(set-register ?l (cons 'file (concat dropbox-path "/projects/TextMunger/Library/")))
(set-register ?o (cons 'file (concat dropbox-path "/projects/TextMunger/Output/")))
;;; setting bookmarks is probably better suited to the below
(set-register ?s '(file . "d:/HOME/Personal/Journal/2013-03/2013-03-03-Sun.jnl"))


;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; DISPLAY

;; window maximized
;; (when (fboundp 'w32-send-sys-command)
;;  (w32-send-sys-command #xf030))
(setq default-frame-alist '((width . 120) (height . 45)))

(setq-default truncate-lines t)

;; load calendar
;; so part of revived-config
(calendar)

;; list bookmarks
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*") ;; removed from default display. when? why? don't really need it? hrm...


;; ;; http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
;; ;; remember windows-configurations
;; ;; also see http://stormcoders.blogspot.com/2007/11/restoring-emacs-layout.html
;; (winner-mode 1)


;; http://www.emacswiki.org/cgi-bin/wiki/WindowsMode
;; http://www.gentei.org/~yuuji/software/windows.el
(autoload 'windows "windows")
;;(win:startup-with-window)
;;(define-key ctl-x-map "C" 'see-you-again)


;; ;; elscreen stuff
;; ;; prefix keys seemed mapped elsewhere if CUA mode set
;; ;; see menu at top
;; ;; http://www.morishima.net/~naoto/software/elscreen/
(add-to-list 'load-path (concat dropbox-site-lisp "apel-10.7/"))
(add-to-list 'load-path (concat dropbox-site-lisp "elscreen-1.4.6/"))
(load "elscreen" "ElScreen" t)

;;; http://www.emacswiki.org/emacs/WThreeMTables
(standard-display-ascii ?\205 "...")
(standard-display-ascii ?\221 [?\'])
(standard-display-ascii ?\222 [?\'])
(standard-display-ascii ?\223 [?\"])
(standard-display-ascii ?\224 [?\"])
(standard-display-ascii ?\225 [?\*])
(standard-display-ascii ?\226 "---")
(standard-display-ascii ?\227 "--")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: move to display section
(global-font-lock-mode 1) ;for all buffers


;; ;; okay, so let's just go with some simple things
;;; use list-colors-display for available colors....
(set-background-color "black")
(set-foreground-color "cyan")
(set-cursor-color "yellow")
(set-face-foreground 'font-lock-warning-face "DeepPink")
(set-face-foreground 'font-lock-string-face "DeepPink")
(set-face-foreground 'font-lock-comment-face "yellow")
(set-face-background 'highlight "grey8")
(set-face-background 'region "blue")
(transient-mark-mode t) ;; otherwise highlighting-region won't show up..




;; Change title bar to ~/file-directory if the current buffer is a
;; real file or buffer name if it is just a buffer.
(setq frame-title-format
      '(:eval
        (if buffer-file-name
            (replace-regexp-in-string
             (getenv "HOME") "~"
             (file-name-directory buffer-file-name))
          (buffer-name))))


;; TODO: winify the path, optionally pass in a path? if not done, use current dir
(defun find-in-directory (field)
  "search in files of current directory.
Assumes you are in dired mode, and you in a directory of keyed (debug) files."
  (interactive "sSearch for: ")
  (setq grep-find-template "find \"<D>\" <X> -type f -name <F> -print0 | xargs -0 grep <C> -n <R> ")
  (rgrep field "*.*" (dired-current-directory)))

(defun play-gomoku ()
  "start gomoku with my preferred size"
  (interactive)
  (gomoku 15 15))



;; restore all buffers from previous config
;; see revive.el
;; http://www.gentei.org/~yuuji/software/revive.el
(require 'revive)
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(server-start)
(resume)
