;;; Package --- elixir-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :ensure t
  :hook subword-mode
  :hook (before-save . lsp-format-buffer))

(use-package mix :ensure t)
(use-package exunit
  :ensure t
  :bind
  ("C-c , a" . exunit-verify-all)
  ("C-c , s" . exunit-verify-single)
  ("C-c , v" . exunit-verify)
  ("C-c , r" . exunit-rerun)
  ("C-c , t" . exunit-toggle-file-and-test))

