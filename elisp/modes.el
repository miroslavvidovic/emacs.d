;;; modes.el --- Miroslav Vidović emacs modes file
;;; Commentary:
;;

;;; Code:

;; Extensible and user friendly startup screen
(use-package dashboard
    :ensure t
    :init
    ;; The title
    (setq dashboard-banner-logo-title "MIROSLAV VIDOVIĆ")
    ;; Customize what will be displayed
    (setq dashboard-items '((recents . 15)))
    :config
    (dashboard-setup-startup-hook))

;; Hide mode info from mode(status) line. Used as a macro in use-package
(use-package diminish
    :ensure t)

;; General for keybindings
(use-package general
    :ensure t
    :init
    (setq my-leader "č")
    :config
    (general-evil-setup))

;; General keybindings - not mode based
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
(general-nmap :prefix my-leader
              "gz" 'zeal-at-point)
(general-nmap :prefix my-leader
              "df" 'dired)

;; Make emacs behave like Vim
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
    :config
    (evil-mode t))

;; Just like vim surround
(use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode t))

;; Vim commenting function
(use-package evil-nerd-commenter
    :ensure t
    :config
    (define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map "gcc" 'evilnc-comment-or-uncomment-lines))

;; Persist search highlighting
(use-package evil-search-highlight-persist
    :ensure t
    :config
    (global-evil-search-highlight-persist t)
    :general
    (general-nmap :prefix my-leader
                  "cl" 'evil-search-highlight-persist-remove-all))

;; Auto pair braces and quotes
(use-package smartparens-config
    :ensure smartparens
    :diminish smartparens-mode
    :config
    (smartparens-global-mode t))

;; Jump to things
(use-package avy
    :general
    (general-nmap :prefix my-leader
                  "q" 'avy-goto-word-1)
    (general-nmap :prefix my-leader
                  "l" 'avy-goto-line)
    :ensure t)

;; Spaceline
(use-package spaceline-config
    :disabled
    :ensure spaceline
    :config
    (spaceline-emacs-theme))

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

;; Alternative to gitgutter and fringe
(use-package diff-hl
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
    (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; Step thorough git history
(use-package git-timemachine
    :ensure t)

;; Interact with git from emacs
(use-package magit
    :ensure t)

;; Snippets
(use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :config
    (yas-global-mode 1))

;; Clean white spaces
(use-package whitespace-cleanup-mode
    :disabled
    :ensure t)

;; Syntax checking
(use-package flycheck
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "e" 'flycheck-list-errors)
    :config
    (global-flycheck-mode))

;; Undo tree
(use-package undo-tree
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "uv" 'undo-tree-visualize)
    :diminish undo-tree-mode)

;; Indent guides 
(use-package highlight-indent-guides
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "ig" 'highlight-indent-guides-mode)
    :init
    (setq highlight-indent-guides-method 'column)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Rainbow color delimiters
(use-package rainbow-delimiters
    :general
    (general-nmap :prefix my-leader
                  "rb" 'rainbow-delimiters-mode)
    :ensure t)

;; NerdTree clone
(use-package neotree
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "m" 'neotree-toggle)
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

;; Alternative to tagbar
(use-package sr-speedbar
    :ensure t)

;; Project interaction
(use-package projectile
    :ensure t
    :init 
    (projectile-mode))

;; Counsel + projectile
(use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on))

;; Used for sorting in ivy
(use-package flx
    :ensure t)

;; Ivy 
(use-package ivy
    :ensure t
    :init
    :diminish ivy-mode
    :general
    (general-nmap :prefix my-leader
                  "ss" 'swiper)
    (general-nmap :prefix my-leader
                  "bb" 'ivy-switch-buffer)
    :config
    (ivy-mode t)
    (setq ivy-use-virtual-buffers t)
    ;; Enable fuzzy matching for results
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))
    (setq enable-recursive-minibuffers t))

(use-package counsel
    :ensure t
    :general
    (general-nmap :prefix my-leader
                  "a" 'counsel-ag)
    (general-nmap "C-p" 'counsel-find-file)
    ;; Use counsel for M-x
    :bind ("M-x" . counsel-M-x))

;; Line numbers
(use-package nlinum
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'nlinum-mode))

;; Autocompletion
(use-package auto-complete
    :ensure t
    :init
    (setq ac-auto-start nil
          ac-disable-inline t
          ac-ignore-case t
          )
    :config
    (ac-config-default)
    (ac-flyspell-workaround)
    (ac-set-trigger-key "TAB")

;;; Language specific
;; PHP
(use-package php-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.php$" . php-mode)))

;; C, C++
(use-package cc-mode
    :ensure t)

;; Web
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

;; Haskell
(use-package haskell-mode
    :ensure t)

;; Python
(use-package python-mode
    :ensure t
    :init
    (autoload 'python-mode "python-mode" "Python Mode." t)
    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
    (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

;; Jedi
(use-package jedi
    :ensure t
    :disabled
    :init
    (add-hook 'python-mode-hook 'jedi:setup)
    :config 
    (setq jedi:complete-on-dot t))
    ;; After this run jedi:install-server in emacs

;; Javascript
(use-package js2-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;;; Clojure
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
    (add-hook 'cider-repl-mode-hook 'paredit-mode) )

(use-package paredit
    :ensure t)

(provide 'modes)

;;; modes.el ends here
