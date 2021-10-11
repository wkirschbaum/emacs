;;; Package --- elixir-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :ensure t
  :config
  (global-subword-mode t)
  :hook (before-save . lsp-format-buffer))

;; (use-package erlang
;;   :ensure t)

(use-package mix :ensure t)
(use-package exunit
  :ensure t
  :bind
  ("C-c , a" . exunit-verify-all)
  ("C-c , s" . exunit-verify-single)
  ("C-c , v" . exunit-verify)
  ("C-c , r" . exunit-rerun)
  ("C-c , t" . exunit-toggle-file-and-test))


(use-package polymode
  :ensure t
  :mode ("\.ex$" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode))
  )
(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))
