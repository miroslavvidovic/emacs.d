;; Highlights matching parenthesis (pairs)
(show-paren-mode 1)

;; Blinking parenthesis
(setq blink-matching-paren-distance nil)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use 4 spaces for a tab
(setq tab-width 4)

;; Add newline at buffer end
(setq next-line-add-newlines t)

;; Require a newline at the end of a file
(setq require-final-newline 't)

;; Disable line truncating
(setq-default truncate-lines 1)

;; Emacs will not automatically add new lines
(setq next-line-add-newlines nil)
