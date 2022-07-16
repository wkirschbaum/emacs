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
        "bootstrap"
        "bootstrap-package"
        "compilation-mode"
        "dired"
        "elfeed"
        "extras"
        "functions"
        "git"
        "lang-docker"
        "lang-elixir"
        "lang-erlang"
        "lang-javascript"
        "lang-markdown"
        "lang-php"
        "lang-terraform"
        "lang-web"
        "lang-yaml"
        "native"
        "org"
        "prog-mode"
        "read-complete"
        "styling"
        "treesitter"
        "irc"
        "local"))

(let ((config-path "~/.config/emacs/modules/"))
  (dolist (module whk/modules)
    (condition-case err
        (load (concat "~/.config/emacs/modules/" module ".el"))
      (error (display-warning "%s" (error-message-string err))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
