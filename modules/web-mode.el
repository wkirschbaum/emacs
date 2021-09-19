(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.[hl]?eex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-css-colorization t))

(use-package emmet-mode
  :ensure t
  :hook web-mode
  :hook css-mode)


;; (straight-use-package
;;  '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

;; (use-package lsp-tailwindcss)
