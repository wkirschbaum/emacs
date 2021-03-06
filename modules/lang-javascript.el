(setq js-indent-level 2)

(use-package typescript-mode
  :mode (("\\.js\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)
         ("\\.ts\\'" . typescript-mode))
  :ensure t
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
