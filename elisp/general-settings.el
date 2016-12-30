;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

;; GUI settings
;------------------------------------------------------------------------------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))         ; Disable the menu bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))         ; Disable the tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))     ; Disable the scroll bar

(setq inhibit-startup-message t                          ; Hide the welcome screen (splash screen)
inhibit-startup-echo-area-message t) 

(set-face-attribute 'default nil                         ; Font settings
                    :family "DejaVu Sans Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(if (daemonp)                                            ; Load the theme 
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (select-frame frame)
            (load-theme 'gruvbox t)))
    (load-theme 'gruvbox t))

(global-linum-mode 1)                                    ; Display line numbers
(setq linum-format "%4d")                                ; Format the line numbers

(setq case-fold-search t)   ; make searches case insensitive

;; Backup files settings
;------------------------------------------------------------------------------
(setq make-backup-files nil)                             ; Stop creating backup~ files
(setq auto-save-default nil)                             ; Stop creating #autosave# files

;; Tabs, spaces, lines and parenthesis
;------------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)                      ; Use spaces instead of tabs
(setq tab-width 4)                                       ; Length of tab is 4 SPC
; (setq truncate-partial-width-windows nil)                ; Don't truncate long lines
(setq next-line-add-newlines t)                          ; Add newline when at buffer end
(setq require-final-newline 't)                          ; Always newline at end of file
(global-linum-mode 1)                                    ; Show line numbers on buffers
(show-paren-mode 1)                                      ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil)                 ; Blinking parenthesis

;; Disable lines truncate
(setq-default truncate-lines 1)

;; Emacs will not automatically add new lines
 (setq next-line-add-newlines nil)

;; Calendar settings
;------------------------------------------------------------------------------
(setq european-calendar-style 't)              ; European style calendar
(setq calendar-week-start-day 1)               ; Week starts on Monday
