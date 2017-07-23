;; Extensible and user friendly startup screen
(use-package dashboard
    :ensure t
    :init
    ;; The title
    (setq dashboard-banner-logo-title "Miroslav Vidović emacs configuration")
    (setq dashboard-startup-banner 'logo)
    ;; Customize what will be displayed
    (setq dashboard-items '((recents . 15)))
    :config
    (dashboard-setup-startup-hook))

(provide 'init-dashboard)
