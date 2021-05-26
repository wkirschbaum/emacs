(use-package haskell-mode
  :ensure t
  :hook (haskell-unicode-input-method-enable)
  :bind
  ("C-c C-c" . haskell-compile)
  :config
  (setq haskell-process-type 'stack-ghci)
  (setq haskell-stylish-on-save t))
