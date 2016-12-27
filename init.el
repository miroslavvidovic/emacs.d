;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

;; Package management section
(require 'package)

; Sources of emacs packages
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

; Ensure that required packages are installed
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; Ensure that required packages are installed 
(package-initialize)

; List of required packages
(ensure-package-installed 'evil 
                          'evil-leader
                          'evil-search-highlight-persist
                          'evil-surround
                          'helm
                          'highlight-indentation
                          'powerline
                          'neotree
                          'hlinum
                          'projectile
                          'auto-complete
                          'flycheck
                          'magit)

;; GUI settings
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))         ; Disable the menu bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))         ; Disable the tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))     ; Disable the scrool bar
(setq inhibit-startup-message t                          ; Hide the welcome screen (splash screen)
inhibit-startup-echo-area-message t) 
(global-linum-mode 1)                                    ; Display line numbers
(setq linum-format "%4d")                                ; Format the line numbers
(hlinum-activate)                                        ; Highlight the current line number

(custom-set-faces                                        ; Custom font
 '(default ((t (:height 120 :family "Inconsolata"))))) 

;; Backup files settings
(setq make-backup-files nil)                             ; Stop creating backup~ files
(setq auto-save-default nil)                             ; Stop creating #autosave# files

;; Tabs, spaces, lines and parethesis
(setq-default indent-tabs-mode nil)            ; Use spaces instead of tabs
(setq tab-width 4)                             ; Length of tab is 4 SPC
(setq truncate-partial-width-windows nil)      ; Don't truncate long lines
(setq next-line-add-newlines t)                ; Add newline when at buffer end
(setq require-final-newline 't)                ; Always newline at end of file
(global-linum-mode 1)                          ; Show line numbers on buffers
(show-paren-mode 1)                            ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil)       ; Blinking parenthesis

;; Calendar settings
(setq european-calendar-style 't)              ; European style calendar
(setq calendar-week-start-day 1)               ; Week starts monday

(ac-config-default)
(powerline-default-theme)

;; Evil mode
(evil-mode t)                   ; Enable evil mode 

(global-evil-leader-mode)       ; Enable evil leader key 
(evil-leader/set-leader "č")    ; Set the evil mode leader key

;; Mappings with leader
(evil-leader/set-key
  "ig" 'highlight-indentation-mode  ; Toggle indent guides with leader+ig
  "b"  'switch-to-buffer
  "lb" 'helm-buffers-list
  "nt" 'neotree-toggle
  "ff" 'projectile-find-file
  "o"  'find-file
)

;; Evil mode vim colors for the cursor
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; Evil mode vim like search highlighting
(global-evil-search-highlight-persist t)

; Enable global flyechecking (file linter plugin)
( add-hook 'after-init-hook #'global-flycheck-mode )

;; Theme settings
(load-theme 'gruvbox t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO: Move the section to the top
;; Add elisp conf files to path
(add-to-list 'load-path "~/.emacs.d/elisp")
;; Load a library from the path
; (load-library "style")
