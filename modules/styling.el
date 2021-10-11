;; (use-package minions
;;   :ensure t
;;   :config
;;   (setq minions-mode-line-lighter "{*}"
;;         minions-direct '(projectile-mode flycheck-mode))
;;   (minions-mode 1))

(use-package modus-themes
  :ensure t
  :init
  ;; Add all your customizations prior to loading the themes
  ;; (setq modus-themes-italic-constructs t
  ;;       modus-themes-bold-constructs nil
  ;;       modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi))

;; (load-theme 'tango-dark)
