(use-package company
  :ensure t
  :config
  (setq company-idle-delay 1
        company-minimum-prefix-length 0
        company-selection-wrap-around t)
  (global-company-mode))
