;;; init.el --- Miroslav Vidović emacs init
;;; Commentary:
;;

;;; Code:

;;----------------------------------------------------------------------------
;;;; PACKAGE MANAGEMENT
;;----------------------------------------------------------------------------

(require 'package)

;; Enable packages only when they are required not on startup
(setq package-enable-at-startup nil)

;; Define package repositories
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))

;; Load and activate emacs packages
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
    (load custom-file))

;; Add a directory to the load path so that when things are `loaded`
;; below, Emacs knows where to look for the corresponding file
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;;;; GENERAL SETTINGS
;;----------------------------------------------------------------------------
;; These settings include backup files settings, date formats
;; and other hard to categorize customizations

;; Make searches case insensitive
(setq case-fold-search t)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup and autosave files
;; stop creating backup~ files
(setq make-backup-files t)
;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; stop creating #autosave# files
;; (setq auto-save-default nil)

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)

;; Calendar settings
;; european style calendar
(setq european-calendar-style 't)
;; week starts on Monday
(setq calendar-week-start-day 1)

;;----------------------------------------------------------------------------
;;;; USER INTERFACE
;;----------------------------------------------------------------------------
;; These settings change the way emacs looks and disable/enable
;; some user interface elements

;; Disable the menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Disable the tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable the scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Hide the welcome screen (splash screen)
(setq inhibit-startup-message t
    inhibit-startup-echo-area-message t) 

;; Font settings
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 107
                    :weight 'normal
                    :width 'normal)

;; Custom theme path
;; user-emacs-directory is ~/.emacs.d
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(add-to-list 'custom-theme-load-path custom-theme-directory)

;; Load the theme 
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (select-frame frame)
            (load-theme 'dracula t)))
    (load-theme 'dracula t))

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")

;;----------------------------------------------------------------------------
;;;; EDITING
;;----------------------------------------------------------------------------
;; Make editing a bit nicer

;; Highlights matching parenthesis (pairs)
(show-paren-mode 1)

;; Blinking parenthesis
(setq blink-matching-paren-distance nil)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use 2 spaces for a tab globally
(setq tab-width 2)

;; Add newline at buffer end
(setq next-line-add-newlines t)

;; Require a newline at the end of a file
(setq require-final-newline 't)

;; Disable line truncating
(setq-default truncate-lines 1)

;; Emacs will not automatically add new lines
(setq next-line-add-newlines nil)

;; UTF8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;----------------------------------------------------------------------------
;;;; FUNCTIONS
;;----------------------------------------------------------------------------
;; Load user defined functions from path
(load-library "functions")

;;----------------------------------------------------------------------------
;;;; DASHBOARD
;;----------------------------------------------------------------------------
;; Extensible and user friendly startup screen

(use-package dashboard
    :ensure t
    :init
    ;;; The title
    (setq dashboard-banner-logo-title "Miroslav Vidović emacs configuration")
    (setq dashboard-startup-banner 'logo)
    ;;; Customize what will be displayed
    (setq dashboard-items '((recents . 15)))
    :config
    (dashboard-setup-startup-hook))

;;----------------------------------------------------------------------------
;;;; SMART MODE LINE (SML)
;;----------------------------------------------------------------------------
;; Best mode line for emacs (looks vs speed)

;; Smart mode line powerline theme
(use-package smart-mode-line-powerline-theme
    :ensure t)

;; Smart mode line
(use-package smart-mode-line
    :ensure t
    :init
    (setq sml/theme 'powerline)
    (setq sml/no-confirm-load-theme t)
    :config
    (sml/setup))

;;----------------------------------------------------------------------------
;;;; DIMINISH
;;----------------------------------------------------------------------------
;; Hide mode info from mode(status) line. Used as a macro in use-package

(use-package diminish
    :ensure t)

;;----------------------------------------------------------------------------
;;;; GENERAL
;;----------------------------------------------------------------------------
;; More convenient key definitions in emacs

(use-package general
    :ensure t
    :init
    (setq my-leader "č")
    :config
    (general-evil-setup))

;;----------------------------------------------------------------------------
;;;; EVIL
;;----------------------------------------------------------------------------
;; Modes that make emacs behave like Vim

(use-package evil
    :ensure t
    :init
    ;; Evil mode cursor colors
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))
    ;; Move between panes with control-direction like in vim-tmux
    :general
    (general-nmap "C-h" 'evil-window-left)
    (general-nmap "C-j" 'evil-window-down)
    (general-nmap "C-k" 'evil-window-up)
    (general-nmap "C-l" 'evil-window-right)
    (general-nmap "C-s" 'save-buffer)
    (general-imap "C-s" 'save-buffer)

    (general-nmap :prefix my-leader
                  "gk" 'general-describe-keybindings)

    ;; General keybindings with leader - not mode based
    (general-nmap :prefix my-leader
                  "kb" 'kill-buffer)
    (general-nmap :prefix my-leader
                  "x" 'delete-window)
    (general-nmap :prefix my-leader
                  "sh" 'shell-command)
    (general-nmap :prefix my-leader
                  "te" 'term)
    (general-nmap :prefix my-leader
                  "vo" 'find-tag-other-window)
    (general-nmap :prefix my-leader
                  "tg" 'create-tags)
    (general-nmap :prefix my-leader
                  "cc" 'compile)
    :config
    (evil-mode t))

;; Evil surround
;; This package emulates surround.vim by Tim Pope
(use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode t))

;; Evil commenter
;; Vim commenting function
(use-package evil-nerd-commenter
    :ensure t
    :config
    (define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map "gcc" 'evilnc-comment-or-uncomment-lines))

;; Evil search
;; Persist search highlighting
(use-package evil-search-highlight-persist
    :ensure t
    :config
    (global-evil-search-highlight-persist t)
    :general
    (general-nmap :prefix my-leader
                  "cl" 'evil-search-highlight-persist-remove-all))

;;----------------------------------------------------------------------------
;;;; DIRED
;;----------------------------------------------------------------------------
;; Default file explorer

(use-package dired
  :defer t
  :config (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :general
  (general-nmap :prefix my-leader
                "df" 'dired))

;;----------------------------------------------------------------------------
;;;; SMARTPARENS
;;----------------------------------------------------------------------------
;; Auto pair braces and quotes

(use-package smartparens-config
    :ensure smartparens
    :diminish smartparens-mode
    :config
    (smartparens-global-mode t))

;;----------------------------------------------------------------------------
;;;; AVY
;;----------------------------------------------------------------------------
;; Jump to things

(use-package avy
    :general
    (general-define-key
                  "ć" 'avy-goto-char-2)
    (general-nmap :prefix my-leader
                  "l" 'avy-goto-line)
    :ensure t)

;;----------------------------------------------------------------------------
;;;; DIFF-HL
;;----------------------------------------------------------------------------
;; Highlighting uncommitted changes 

(use-package diff-hl
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
    (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;;----------------------------------------------------------------------------
;;;; GIT TIMEMACHINE
;;----------------------------------------------------------------------------
;; Step through historic versions of git controlled files

(use-package git-timemachine
    :ensure t)

;;----------------------------------------------------------------------------
;;;; MAGIT
;;----------------------------------------------------------------------------
;; Interact with git from emacs

(use-package magit
    :ensure t)

;;----------------------------------------------------------------------------
;;;; YASNIPPET
;;----------------------------------------------------------------------------
;; Snippets

(use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :config
    (yas-global-mode 1))

;;----------------------------------------------------------------------------
;;;; WHITESPACE CLEANUP
;;----------------------------------------------------------------------------
;; Clean trailing white spaces

(use-package whitespace-cleanup-mode
    :disabled
    :ensure t)

;;----------------------------------------------------------------------------
;;;; UNDO TREE
;;----------------------------------------------------------------------------
;; Undo tree visualization

(use-package undo-tree
    :ensure t
    :config
    (progn
      (setq undo-tree-visualizer-timestamps t)
      (setq undo-tree-visualizer-diff t))
    :general
    (general-nmap :prefix my-leader
                  "uv" 'undo-tree-visualize)
    :diminish undo-tree-mode)

;;----------------------------------------------------------------------------
;;;; INDENT GUIDES
;;----------------------------------------------------------------------------
;; Higlight indentation

(use-package highlight-indent-guides
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "ig" 'highlight-indent-guides-mode)
    :init
    (setq highlight-indent-guides-method 'column)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;----------------------------------------------------------------------------
;;;; RAINBOW DELIMITERS
;;----------------------------------------------------------------------------
;; Rainbow color delimiters

(use-package rainbow-delimiters
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "rb" 'rainbow-delimiters-mode))

;;----------------------------------------------------------------------------
;;;; NEOTREE
;;----------------------------------------------------------------------------
;; NerdTree clone

(use-package neotree
    :ensure t
    :defer t
    :general
    (general-nmap :prefix my-leader
                  "n" 'neotree-toggle)
    :init
    (add-hook 'neotree-mode-hook
        (lambda ()
          (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
          (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
          (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
          (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
          (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
          (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
          (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
          (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
          (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
          (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node))))

;;----------------------------------------------------------------------------
;;;; SR SPEEDBAR
;;----------------------------------------------------------------------------
;; Alternative to tagbar

(use-package sr-speedbar
    :ensure t)

;;----------------------------------------------------------------------------
;;;; NLINUM
;;----------------------------------------------------------------------------
;; Line numbers

(use-package nlinum
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'nlinum-mode))

;;----------------------------------------------------------------------------
;;;; IVY
;;----------------------------------------------------------------------------
;; Generic completion frontend, faster than helm

;; Small package used for sorting in ivy, needs to be before ivy
(use-package flx
    :ensure t)

;; Ivy
(use-package ivy
    :ensure t
    :diminish ivy-mode
    :general
    (general-nmap :prefix my-leader
                  "ss" 'swiper)
    (general-nmap :prefix my-leader
                  "b" 'ivy-switch-buffer)
    :config
    (ivy-mode t)
    (setq ivy-use-virtual-buffers t)
    ;; Enable fuzzy matching for results
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))
    (setq enable-recursive-minibuffers t))

;;----------------------------------------------------------------------------
;;;; COUNSEL
;;----------------------------------------------------------------------------
;; Ivy enahnced versions of emacs commands

(use-package counsel
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "a" 'counsel-ag)
    (general-nmap "C-p" 'counsel-projectile-find-file)
    (general-nmap :prefix my-leader
                  "." 'counsel-find-file)
    (general-nmap :prefix my-leader
                  "im" 'counsel-imenu)
    ;; Use counsel for M-x
    :bind ("M-x" . counsel-M-x))

;;----------------------------------------------------------------------------
;;;; PROJECTILE
;;----------------------------------------------------------------------------
;; Project interaction library 

(use-package projectile
    :ensure t
    :init
    (projectile-mode))

;; Use projectile from counsel
(use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on))

;;----------------------------------------------------------------------------
;;;; ZEAL AT POINT
;;----------------------------------------------------------------------------
;; Open the word under cursor in Zeal

(use-package zeal-at-point
  :load-path "~/.emacs.d/elisp/zeal-at-point.el"
  :general
  (general-nmap :prefix my-leader
                "gz" 'zeal-at-point))

;;----------------------------------------------------------------------------
;;;; FLYCHECK
;;----------------------------------------------------------------------------
;; Syntax checking

(use-package flycheck
    :ensure t
    :defer 5
    :general
    (general-nmap :prefix my-leader
                  "e" 'flycheck-list-errors)
    :config
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-shellcheck-follow-sources nil)
    (global-flycheck-mode))
    ; Fix error for older shellcheck versions

;; Flycheck tips and hints in a popup window
(use-package flycheck-pos-tip
    :ensure t
    :config
    (setq flycheck-pos-tip-timeout 10)
    (setq flycheck-display-errors-delay 0.5)
    (flycheck-pos-tip-mode +1))

;;----------------------------------------------------------------------------
;;;; COMPANY AUTOCOMPLETE
;;----------------------------------------------------------------------------
;; Autocompletion mode

(use-package company
    :ensure t
    :diminish company-mode
    :defer 2
    :config 
    (global-company-mode)
    (setq company-dabbrev-ignore-case nil
          company-idle-delay 0.2
          ;; press M-number to choose candidate
          company-show-numbers t
          ;; make previous/next selection in the popup cycles
          company-selection-wrap-around t
          company-minimum-prefix-length 2
          company-tooltip-limit 10
          company-require-match nil)

  ;; Cycle possible completions with tab
  (eval-after-load 'company
  '(progn
    ; (define-key company-active-map (kbd "C-n") 'company-select-next)
    ; (define-key company-active-map (kbd "C-p") 'company-select-previous)
    ; (define-key company-search-map (kbd "C-n") 'company-select-next)
    ; (define-key company-search-map (kbd "C-p") 'company-select-previous)
    ; (define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering))))
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (add-hook 'python-mode-hook 'my/python-mode-hook))))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

;; Quick help window for possible completions
(use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1))

;; Jedi backend for company
(use-package company-jedi
  :ensure t)

;;----------------------------------------------------------------------------
;;;; HTTP REST
;;----------------------------------------------------------------------------
;; HTTP REST client tool for emacs

(use-package restclient
  :ensure t)

;;----------------------------------------------------------------------------
;;;; MARKDOWN
;;----------------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;----------------------------------------------------------------------------
;;;; PHP
;;----------------------------------------------------------------------------

(use-package php-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.php$" . php-mode)))

;;----------------------------------------------------------------------------
;;;; C, C++
;;----------------------------------------------------------------------------

(use-package cc-mode
    :ensure t)

;;----------------------------------------------------------------------------
;;;; WEB
;;----------------------------------------------------------------------------

(use-package web-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    ; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

;;----------------------------------------------------------------------------
;;;; HASKELL
;;----------------------------------------------------------------------------

(use-package haskell-mode
    :ensure t)

;;----------------------------------------------------------------------------
;;;; PYTHON
;;----------------------------------------------------------------------------

(use-package python-mode
    :ensure t
    :config
    (autoload 'python-mode "python-mode" "Python Mode." t)
    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
    (add-to-list 'interpreter-mode-alist '("python" . python-mode))
    (setq-local tab-width 4))

;;----------------------------------------------------------------------------
;;;; JAVASCRIPT
;;----------------------------------------------------------------------------

(use-package js2-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;;----------------------------------------------------------------------------
;;;; JSON
;;----------------------------------------------------------------------------
;; Json editing mode

(use-package json-mode
    :ensure t)

;;----------------------------------------------------------------------------
;;;; CLOJURE
;;----------------------------------------------------------------------------

(use-package clojure-mode
    :ensure t
    :init
    ;; Use clojure mode for other extensions
    (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))
    :config
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'clojure-mode-hook 'subword-mode))

(use-package clojure-mode-extra-font-locking
    :ensure t)

;; Better matching parens for lisp family languages
(use-package paredit
    :ensure t)

;; REPL for clojure
(use-package cider
    :ensure t
    :init
    ;; go right to the REPL buffer when it's finished connecting
    (setq cider-repl-pop-to-buffer-on-connect t)
    ;; When there's a cider error, show its buffer and switch to it
    (setq cider-show-error-buffer t)
    (setq cider-auto-select-error-buffer t)
    ;; Where to store the cider history.
    (setq cider-repl-history-file "~/.emacs.d/cider-history")
    ;; Wrap when navigating history.
    (setq cider-repl-wrap-history t)
    :config
    ;; provides minibuffer documentation for the code you're typing into the repl
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    ;; enable paredit in your REPL
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    ;; enable fuzzy completion
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

;;----------------------------------------------------------------------------
;;;; SHELL
;;----------------------------------------------------------------------------
;; Shell mode stuff

;; Associate file wiith sh mode
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
(setq-local tab-width 4)

;;----------------------------------------------------------------------------
;;;; CSV
;;----------------------------------------------------------------------------
(use-package csv-mode
  :ensure t)

(provide 'init)

;;; init.el ends here
