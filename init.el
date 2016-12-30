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

; Activate all the packages 
(package-initialize)

; Update your local package index
(unless package-archive-contents
(package-refresh-contents))

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

; List of required packages
(ensure-package-installed 'evil                                 ; Vim mode in emacs
                          'evil-leader                          ; Vim leader key mode
                          'evil-surround                        ; Vim like surround
                          'evil-search-highlight-persist        ; Higlight search like Vim
                          'evil-nerd-commenter                  ; Easy comment
                          'powerline                            ; Vim style powerline
                          'helm                                 ; Helm
                          'projectile                           ; Projectile
                          'highlight-indentation                ; Indent guidelines
                          'neotree                              ; NerdTree clone
                          'rainbow-delimiters                   ; Rainbow colors
                          'autopair                             ; Auto parenthesis
                          'avy                                  ; Vim easy motion clone
                          'git-gutter                           ; Git changes
                          'auto-complete                        ; Auto-complete functionality
                          'flycheck                             ; Linter
                          'magit                                ; Git interaction
                          'php-mode
                          'elpy
                          'js2-mode
                          'popup
                          'ac-helm
                          'web-mode
                          ; 'ac-php
                          'helm-ag
)

;; Add elisp conf files to path
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "general-settings")
(load-library "modes")
(load-library "custom")
