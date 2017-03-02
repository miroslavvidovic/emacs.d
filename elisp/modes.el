;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

;; Evil mode, plugins and settings
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

; Mappings with leader
(evil-leader/set-key
  "ig" 'highlight-indent-guides-mode          ; Toggle indent guides with leader+ig
  "b"  'helm-buffers-list;                  ; List buffers
  "m"  'neotree-toggle                      ; Toggle the neo tree
  "ff" 'projectile-find-file                ; Fuzzy find with projectile
  "o"  'helm-find-files                     ; Open a file with helm
  "rb" 'rainbow-delimiters-mode             ; Toggle rainbow delimiters
  "ci" 'evilnc-comment-or-uncomment-lines   ; Comment selected lines
  "q"  'avy-goto-word-1                     ; Easy motion to word
  "l"  'avy-goto-line                       ; Easy motion to line
  "e"  'flycheck-list-errors                ; List all errors in the buffer
)

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
(powerline-center-theme)                                

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
; ; NeoTree keybindings with evil mode
; (global-set-key [f8] 'neotree-toggle)

; (setq projectile-switch-project-action 'neotree-projectile-action)
;   (add-hook 'neotree-mode-hook
;     (lambda ()
;       (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
;       (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
;       (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
;       (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
;       (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
;       (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
;       (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

;       (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
;       (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

;       (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

; Helm
;------------------------------------------------------------------------------
; ;; Enable helm with default settings
(require 'helm-config)

; Javascript mode js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

; Python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode))
; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
;; After this run jedi:install-server in emacs

; ;; auto complete default
; ; (global-auto-complete-mode t)
; ; (ac-config-default)

; ;; web mode
; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

; ;; Display auto-complete in helm with ctrl-space
; ; (global-set-key (kbd "C-<SPC>") 'ac-complete-with-helm)
; ; (define-key ac-complete-mode-map (kbd "C-<SPC>") 'ac-complete-with-helm)

; ; ;;PHP
; ; (require 'php-mode)
; ; (eval-after-load 'company
; ;   '(progn
; ;      (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
; ;      (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

; ; (eval-after-load 'company
; ;       (lambda ()
; ;         (set-face-attribute
; ;          'company-preview
; ;          nil
; ;          :background (face-attribute 'company-preview-common :background))))
