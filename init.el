;------------------------------------------------------------------------------
;
; Miroslav Vidović
;
; emacs configuration
;
;------------------------------------------------------------------------------

; Package management section
(require 'package)

; List of used packages
(setq package-list '(evil                                 ; Vim mode in emacs
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
                     rainbow-delimiters                   ; Rainbow colors
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

                     ; Programming modes
                     cc-mode                              ; Mode for c
                     php-mode                             ; Mode for php
                     js2-mode                             ; Mode for javascript
                     python-mode                          ; Mode for python
                     jedi                                 ; Autocomplete for python
                     haskell-mode                         ; Mode for haskell
                     json-mode                            ; Mode for json
                     web-mode                             ; Mode for html...

                     ;Git
                     git-gutter                           ; Git changes
                     magit                                ; Git interaction
                     git-timemachine                      ; Step through git history
                     ))

; Package repositories
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) 

; Activate all the packages 
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
(load-library "general-settings")
(load-library "functions")
(load-library "modes")
(load-library "custom")
(load-library "zeal-at-point")
