;; Generate the tags file for emacs
;; Path to ctags executable
(setq path-to-ctags "/usr/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
    (format "ctags -f TAGS -e -R %s" dir-name)))

;; Count words
(defun count-words-buffer ( )
  "Count the number of words in the current buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
             (forward-word 1)
             (setq count (1+ count)))
      (message "buffer contains %d words." count))))

;; Insert date
(defun put-the-date ()
  "Execute a shell command (date) and display the output in the current buffer."
  (interactive)
  (insert (shell-command-to-string "date")))

;; Kill all other buffers
(defun kill-other-buffers ()
  "Killall other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

;; Show trailing whitespaces
(defun tf-toggle-show-trailing-whitespace ()
    "Toggle show-trailing-whitespace between t and nil"
    (interactive)
    (setq show-trailing-whitespace (not show-trailing-whitespace)))

;;-----------------------------------------------------------------------------
; Fastest way to explore and edit your emacs config
; http://metasandwich.com/2013/01/19/emacs-config-youre-doing-it-wrong/
;;-----------------------------------------------------------------------------
;; Sections for imenu
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
 
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;; Start imenu with counsel and open the emacs init file for section search
(defun init-imenu (p)

(defun init-narrow-to-section ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^;;;;")
      (re-search-backward "^;;;;" nil t))
    (push-mark)
    (next-line)
    (re-search-forward "^;;;;" nil t)
    (previous-line)
    (narrow-to-region (region-beginning) (region-end))))
  (interactive "P")
  (find-file-existing "~/.emacs.d/init.el")
  (widen)
  (counsel-imenu)
(if p (init-narrow-to-section)))
;;-----------------------------------------------------------------------------
