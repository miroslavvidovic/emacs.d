;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

;; Evil mode, plugins and settings
;------------------------------------------------------------------------------
(evil-mode t)                               ; Enable evil mode 

(global-evil-leader-mode)                   ; Enable evil leader key 
(evil-leader/set-leader "č")                ; Set the evil mode leader key

(global-evil-search-highlight-persist t)    ; Evil mode vim like search highlighting

(global-evil-surround-mode 1)               ; Enable evil surround mode

; Evil mode cursor colors
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

; Mappings with leader
(evil-leader/set-key
  "ig" 'highlight-indentation-mode          ; Toggle indent guides with leader+ig
  "b"  'helm-buffers-list;                  ; List buffers
  "m"  'neotree-toggle                      ; Toggle the neo tree
  "ff" 'projectile-find-file                ; Fuzzy find with projectile
  "o"  'helm-find-files                     ; Open a file with helm
  "rb" 'rainbow-delimiters-mode             ; Toggle rainbow delimiters
  "ci" 'evilnc-comment-or-uncomment-lines   ; Comment selected lines
  "q"  'avy-goto-word-1                     ; Easy motion to word
  "e"  'flycheck-list-errors                ; List all errors in the buffer
)

;; Start powerline with the center theme
(powerline-center-theme)

;; Enable helm with default settings
(require 'helm-config)

; NeoTree keybindings with evil mode
(setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
      (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Enable autopairs in all buffers
(autopair-global-mode) 

;; GitGutter activation
(global-git-gutter-mode +1) 

;; auto complete default
(global-auto-complete-mode t)
(ac-config-default)

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))         ; Enable flycheck globally

;; web mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; Display auto-complete in helm with ctrl-space
(global-set-key (kbd "C-<SPC>") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-<SPC>") 'ac-complete-with-helm)

;; Python 
(when (require 'elpy nil t)
  (elpy-enable))
(setq elpy-rpc-python-command "/usr/bin/python3")
(setq elpy-rpc-backend "jedi")  
; (elpy-use-ipython "/usr/bin/ipython3")