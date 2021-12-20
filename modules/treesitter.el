(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
