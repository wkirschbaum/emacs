(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "{*}"
        minions-direct '(projectile-mode flycheck-mode))
  (minions-mode 1))

(load-theme 'tango-dark)
