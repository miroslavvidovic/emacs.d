;;; init.el --- Miroslav Vidović emacs init 
;;; Commentary:
;;

;;; Code:

;; Package management section
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

;; Modes
(load-library "modes")

;; Open zeal documentation from emacs
(load-library "zeal-at-point")

(provide 'init)

;;; init.el ends here
