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


(use-package gerrit
  :ensure t
  :custom
  (gerrit-host "gerrit")  ;; is needed for REST API calls
  :config
  (progn
    (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload-transient)
    (global-set-key (kbd "C-x o") 'gerrit-download)))
