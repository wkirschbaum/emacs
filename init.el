;; Package --- Personal init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun setup-custom-config (config-path)
  (setq custom-file (concat config-path "custom.el"))
  (if (file-exists-p custom-file)
      (load custom-file)))

(let ((config-path "~/.config/emacs/"))
  (setup-custom-config config-path))

(defvar whk/modules)
(setq whk/modules
      '(
        "bootstrap"
        "exwm"
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
        "terraform-mode"
        "yaml-mode"
        "compilation-mode"
        "javascript-mode"
        "web-mode"
        "styling"
        "extras"
        "functions"))

(let ((config-path "~/.config/emacs/modules/"))
  (dolist (module whk/modules)
    (condition-case err
        (load (concat "~/.config/emacs/modules/" module ".el"))
      (error (display-warning "%s" (error-message-string err))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here

