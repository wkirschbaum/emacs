;; Package --- Personal init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar whk/modules)
(setq whk/modules
      '(
        "bootstrap"
        "native-config"
        "shared"
        "bell"
        "whitespace"
        "dired"
        "org"
        "po-mode"
        "lsp"
        "elixir-mode"
        "docker-mode"
        "markdown-mode"
        "yaml-mode"
        "compilation-mode"
        "styling"))

(let ((config-path "~/.config/emacs/modules/"))
  (dolist (module whk/modules)
    (condition-case err
        (load (concat "~/.config/emacs/modules/" module ".el"))
      (error (display-warning "%s" (error-message-string err))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
