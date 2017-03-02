;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

;; Package management section
(require 'package)

; List the packages you want
(setq package-list '(evil                                 ; Vim mode in emacs
                     evil-leader                          ; Vim leader key mode
                     evil-surround                        ; Vim like surround
                     evil-search-highlight-persist        ; Higlight search like Vim
                     evil-nerd-commenter                  ; Easy comment
                     dracula-theme                        ; Theme dracula
                     gruvbox-theme                        ; Theme gruvbox
                     auto-complete                        ; Autocompletion
                     powerline                            ; Vim style powerline
                     yasnippet                            ; Snippets
                     flycheck                             ; Linters
                     autopair                             ; Auto pair braces and quotes 
                     highlight-indent-guides              ; Indent guidelines
                     rainbow-delimiters                   ; Rainbow colors
                     neotree                              ; NerdTree clone
                     avy                                  ; Vim easy motion clone
                     projectile                           ; Projectile
                     helm                                 ; Helm
                     helm-ag                              ; Ag (silver searcher) in helm

                     ;Modes
                     cc-mode                              ; Mode for c
                     php-mode                             ; Mode for php
                     js2-mode                             ; Mode for javascript
                     python-mode
                     jedi                                 ; Autocomplete for python
                     haskell-mode

                     ;Git
                     git-gutter                           ; Git changes
                     magit                                ; Git interaction
                     ))

; Package repositories
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t) 

; Activate all the packages 
(package-initialize)

; Update your local package index
(unless package-archive-contents
(package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
(package-install package)))

; ; List of required packages
;                           ; 'use-package                          ; Control package loading
;                           ; 'popup
;                           ; 'web-mode
;                           ; 'ac-php
; )

;; Add elisp conf files to path
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "general-settings")
(load-library "modes")
; (load-library "custom")
