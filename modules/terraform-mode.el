(use-package terraform-mode
  :ensure t
  :hook (before-save . terraform-format-buffer))
