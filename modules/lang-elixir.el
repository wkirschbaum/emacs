;; ;;; package --- elixir-mode -*- lexical-binding: t; -*-
;; ;;; Commentary:
;; ;;; Code:

(require 'eglot)

(defvar whk/eglot-enabled nil)
(load "~/src/emacs/heex-ts-mode/heex-ts-mode.el")
(load "~/src/emacs/elixir-ts-mode/elixir-ts-mode.el")

;; (defvar whk/eglot-enabled t)
;; (use-package elixir-ts-mode
;;   :ensure t
;;   :config
;;   (global-subword-mode t))

(setq whk/elixir-lsp-sh "~/src/elixir/elixir-ls/bin/language_server.sh")

(dolist (mode '(elixir-mode elixir-ts-mode heex-ts-mode))
    (add-to-list 'eglot-server-programs `(,mode . (,whk/elixir-lsp-sh))))

(when whk/eglot-enabled
  (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
  (add-hook 'heex-ts-mode-hook 'eglot-ensure))

(use-package mix :ensure t)
(use-package exunit
  :ensure t
  :bind
  ("C-c , a" . exunit-verify-all)
  ("C-c , s" . exunit-verify-single)
  ("C-c , v" . exunit-verify)
  ("C-c , r" . exunit-rerun)
  ("C-c , t" . exunit-toggle-file-and-test))
