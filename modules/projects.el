(use-package projectile
  :ensure t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-file-exists-remote-cache-expire nil
        projectile-completion-system 'default
        projectile-dynamic-mode-line t
        projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  (projectile-mode +1))
