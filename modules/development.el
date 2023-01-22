(use-package package-lint
  :ensure t)

(use-package package-build
  :ensure t)

(use-package flycheck-package
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

