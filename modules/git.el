(use-package magit
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package forge
  :ensure t)

(use-package diff-hl
  :ensure t
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-post-refresh . diff-hl-magit-pre-refresh))
  :config
  (global-diff-hl-mode))

(use-package git-modes
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))
