;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

;; Package management section
(require 'package)

;; Define package repositories
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

;; List of used packages
(defvar package-list
    '(evil                                 ; Vim mode in emacs
      evil-leader                          ; Vim leader key mode
      evil-surround                        ; Vim like surround
      evil-search-highlight-persist        ; Higlight search like Vim
      evil-nerd-commenter                  ; Easy comment

      ; Themes
      dracula-theme                        ; Theme dracula
      gruvbox-theme                        ; Theme gruvbox
      zenburn-theme                        ; Theme zenburn

      autopair                             ; Auto pair braces and quotes 
      auto-complete                        ; Autocompletion
      spaceline                            ; Statusline from spacemacs
      yasnippet                            ; Snippets
      flycheck                             ; Linters
      highlight-indent-guides              ; Indent guidelines
      rainbow-delimiters                   ;; colorful parenthesis matching
      neotree                              ; NerdTree clone
      avy                                  ; Vim easy motion clone
      projectile                           ; Projectile
      helm                                 ; Helm
      helm-ag                              ; Ag (silver searcher) in helm
      ivy                                  ; Lightweight alternative to helm
      counsel                              ; Ivy tools
      dashboard                            ; Welcome screen
      helm-dash                            ; Search dash docsets from emacs
      sr-speedbar                          ; Alternative to tagbar
      perspective                          ; Manage perspectives (workspaces)

      ;; Programming modes
      cc-mode                              ; Mode for c
      php-mode                             ; Mode for php
      js2-mode                             ; Mode for javascript
      python-mode                          ; Mode for python
      jedi                                 ; Autocomplete for python
      haskell-mode                         ; Mode for haskell
      json-mode                            ; Mode for json
      web-mode                             ; Mode for html

      ; Clojure
      paredit                            ;; makes handling lisp expressions much, much easier
      clojure-mode                       ;; key bindings and code colorization for Clojure
      clojure-mode-extra-font-locking    ;; extra syntax highlighting for clojure
      cider                              ;; integration with a Clojure REPL

      ; Git
      git-gutter                         ;; display git diff in gutter
      magit                              ;; interact with git from emacs
      git-timemachine                    ;; step through git history
      ))


;; Load and activate emacs packages
(package-initialize)

; Update local package index
(unless package-archive-contents
    (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
(package-install package)))

; Add elisp conf files to path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/settings")
(load-library "general-settings")
(load-library "user-interface")
(load-library "editing")
(load-library "functions")
(load-library "modes")
(load-library "custom")
(load-library "zeal-at-point")

;; language specific settings
(load-library "setup-clojure")
