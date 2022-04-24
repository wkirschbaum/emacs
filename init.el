;; Package --- Personal init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq native-comp-async-report-warnings-errors nil)

(defun setup-custom-config (config-path)
  (setq custom-file (concat config-path "custom.el"))
  (if (file-exists-p custom-file)
      (load custom-file)))

(let ((config-path "~/.config/emacs/"))
  (setup-custom-config config-path))

(defvar whk/modules)
(setq whk/modules
      '(
        "native"
        "bootstrap-package"
        "bootstrap"
        "gnupgp"
        "shared"
        "registers"
        "paredit"
        "styling"
        "company-mode"
        "git"
        "read-complete"
        "eshell"
        "bell"
        "whitespace"
        "dired"
        "org"
        "po-mode"
        "lsp-mode"
        "elixir-mode"
        ;; "elixir-custom-mode"
        "erlang-mode"
        "docker-mode"
        "markdown-mode"
        "php-mode"
        "terraform-mode"
        "scala-mode"
        "yaml-mode"
        "compilation-mode"
        "javascript-mode"
        "web-mode"
        "ledger"
        "haskell-mode"
        "csharp-mode"
        "rust-mode"
        "elfeed"
        "extras"
        "ebdb"
        "dart-mode"
        "functions"
        "treesitter"
        "slime"
        "experiments"))

(let ((config-path "~/.config/emacs/modules/"))
  (dolist (module whk/modules)
    (condition-case err
        (load (concat "~/.config/emacs/modules/" module ".el"))
      (error (display-warning "%s" (error-message-string err))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
