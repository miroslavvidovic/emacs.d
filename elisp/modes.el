;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

; Evil mode, plugins and settings
;------------------------------------------------------------------------------
(require 'evil)
(evil-mode t)                                 ; Enable evil mode 

(global-evil-leader-mode)                     ; Enable evil leader key 
(evil-leader/set-leader "č")                  ; Set the evil mode leader key

(global-evil-search-highlight-persist t)      ; Evil mode vim like search highlighting

(global-evil-surround-mode 1)                 ; Enable evil surround mode

; Evil mode cursor colors
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

; Move betewen panes with control-direction like in vim-tmux 
(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

; Mappings with leader
(evil-leader/set-key
  "a"  'ag                                  ; Ack like search
  "ig" 'highlight-indent-guides-mode        ; Toggle indent guides with leader+ig
  "kb" 'kill-buffer                         ; Kill - close a buffer
  "b"  'helm-buffers-list;                  ; List buffers
  "m"  'neotree-toggle                      ; Toggle the neo tree
  "ff" 'projectile-find-file                ; Fuzzy find with projectile
  "o"  'helm-find-files                     ; Open a file with helm
  "rb" 'rainbow-delimiters-mode             ; Toggle rainbow delimiters
  "ci" 'evilnc-comment-or-uncomment-lines   ; Comment selected lines
  "q"  'avy-goto-word-1                     ; Easy motion to word
  "l"  'avy-goto-line                       ; Easy motion to line
  "e"  'flycheck-list-errors                ; List all errors in the buffer
  "sh" 'shell-command                       ; Run a shell command for emacs
  "x"  'delete-window                       ; Close a window
  "sp" 'flyspell-mode                       ; Spell checker
  "l"  'evil-search-highlight-persist-remove-all ; Remove highlighted search results
  "d"  'dired ; Dired file manager
  "rn" 'ranger ; ranger file manager
  "t"  'term ; terminal emulator
  "ps" 'perspective-switch                  ; Switch to perspective
  "p"  'counsel-find-file
)

; Vim style keybinding for commenting
(define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map "lf" 'helm-semantic-or-imenu)

; (ivy-mode 1)
; (setq ivy-use-virtual-buffers t)
; (setq enable-recursive-minibuffers t)

;; Autocomplete
;------------------------------------------------------------------------------
(ac-config-default)
; (define-key ac-mode-map (kbd "C-n") 'auto-complete)
(ac-set-trigger-key "TAB")
; (setq ac-auto-start nil)

; Snippets
;------------------------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)

; Powerline
;------------------------------------------------------------------------------
; Start powerline with the center theme
; (powerline-center-theme)                                

; Spaceline
;------------------------------------------------------------------------------
(require 'spaceline-config)
(spaceline-emacs-theme)

; Avy
;------------------------------------------------------------------------------
; Shortcuts for word and line jump in vim evil keybindings

; Flycheck
;------------------------------------------------------------------------------
(require 'flycheck)
(global-flycheck-mode)

; Enable autopairs in all buffers
;------------------------------------------------------------------------------
(autopair-global-mode) 

; Rainbow delimiters
;------------------------------------------------------------------------------
; Not enabled on startup, shortcut for toggle is set up in vim evil keybindings

; projectile
;------------------------------------------------------------------------------
; Not enabled on startup, shortcuts for commands are set up in vim evil keybindings

; Higlight indent guides
;------------------------------------------------------------------------------
; Not enabled on startup, shortcut for enabling is set up in vim evil keybindings
(setq highlight-indent-guides-method 'column)

; GitGutter
;------------------------------------------------------------------------------
; ;; GitGutter activation
(global-git-gutter-mode +1) 

; NeoTree
;------------------------------------------------------------------------------
; NeoTree keybindings with evil mode
(global-set-key [f8] 'neotree-toggle)

(setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

; Helm
;------------------------------------------------------------------------------
; ;; Enable helm with default settings
(require 'helm-config)

; Javascript mode js2-mode
;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

; Python-mode
;------------------------------------------------------------------------------
(autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode))

; Jedi
;------------------------------------------------------------------------------
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
;; After this run jedi:install-server in emacs

; Startup screen dashboard
;------------------------------------------------------------------------------
(require 'dashboard)
(dashboard-setup-startup-hook)

; PHP
;------------------------------------------------------------------------------
(require 'php-mode)
(eval-after-load 'php-mode
  '(require 'php-ext))

; Web Mode
;------------------------------------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
