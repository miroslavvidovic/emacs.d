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
    ;; Evil stuff
    '(evil                                 ;; vim mode for emacs
      evil-leader                          ;; use a leader key like in vim
      evil-surround                        ;; surround just like in vim
      evil-search-highlight-persist        ;; higlight when searching
      evil-nerd-commenter                  ;; comment/uncomment code

      autopair                             ;; auto pair braces and quotes
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

      ;; Git
      git-gutter                         ;; display git diff in gutter
      magit                              ;; interact with git from emacs
      git-timemachine                    ;; step through git history
      ))


;; Load and activate emacs packages
(package-initialize)

;; Update the local package index
(unless package-archive-contents
    (package-refresh-contents))

;; Install all missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
(package-install package)))

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add a directory to the load path so that when things are `loaded`
;; below, Emacs knows where to look for the corresponding file
(add-to-list 'load-path "~/.emacs.d/settings")
(add-to-list 'load-path "~/.emacs.d/elisp")

;; These settings include backup files settings, date formats
;; and other hard to categorize customizations
(load-library "general-settings")

;; These settings change the way emacs looks and disable/enable
;; some user interface elements
(load-library "user-interface")

;; Make editing a bit nicer
(load-library "editing")

;; Custom elisp functions
(load-library "functions")

(load-library "modes")

;; Open zeal documentation from emacs
(load-library "zeal-at-point")

;; Language specific settings
(load-library "setup-clojure")
(load-library "setup-sh")
