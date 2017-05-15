;; Disable the menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Disable the tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable the scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Hide the welcome screen (splash screen)
(setq inhibit-startup-message t
    inhibit-startup-echo-area-message t) 

;; Font settings
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Load the theme 
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (select-frame frame)
            (load-theme 'dracula t)))
    (load-theme 'dracula t))

;; Show line numbers
(global-linum-mode 1)

;; Format the line numbers
(setq linum-format "%4d")

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")
