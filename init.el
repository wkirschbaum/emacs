;; Package --- Personal init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(Xsetq comp-deferred-compilation-deny-list '())
(setq native-comp-async-report-warnings-errors nil)

(defun setup-custom-config (config-path)
  ;; Remove on next upgrade

  (setq custom-file (concat config-path "custom.el"))
  (if (file-exists-p custom-file)
      (load custom-file)))

(let ((config-path "~/.config/emacs/"))
  (setup-custom-config config-path))

(defvar whk/modules)
(setq whk/modules
      '(
        "bootstrap"
        "native"
        "shared"
        "git"
        "read-complete"
        "bell"
        "whitespace"
        "dired"
        "org"
        "projects"
        "po-mode"
        "lsp-mode"
        "elixir-mode"
        "docker-mode"
        "markdown-mode"
        "terraform-mode"
        "yaml-mode"
        "compilation-mode"
        "javascript-mode"
        "web-mode"
        "ledger"
        "haskell-mode"
        "rust-mode"
        "elfeed"
        "styling"
        "extras"
        "functions"
        "secret"))

(let ((config-path "~/.config/emacs/modules/"))
  (dolist (module whk/modules)
    (condition-case err
        (load (concat "~/.config/emacs/modules/" module ".el"))
      (error (display-warning "%s" (error-message-string err))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here

