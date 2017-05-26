;; Make searches case insensitive
(setq case-fold-search t)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup and autosave files 
;; stop creating backup~ files
(setq make-backup-files nil)
;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
; (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
;                                                "backups"))))

;; stop creating #autosave# files
(setq auto-save-default nil)

;; Calendar settings
;; european style calendar
(setq european-calendar-style 't)
;; week starts on Monday
(setq calendar-week-start-day 1)
