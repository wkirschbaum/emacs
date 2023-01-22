;; Package --- Personal init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun setup-custom-config (config-path)
  (setq custom-file (concat config-path "custom.el"))
  (if (file-exists-p custom-file)
      (load custom-file)))

(let ((config-path user-emacs-directory))
  (setup-custom-config config-path))

(defvar whk/modules)
(setq whk/modules
      '(
        "native"
        "minimal"
        "bootstrap"
        "bootstrap-package"
        "compilation-mode"
        "dired"
        "elfeed"
        "extras"
        "functions"
        "git"
        "eshell"
        "lang-docker"
        "lang-elixir"
        "lang-erlang"
        "lang-javascript"
        "lang-markdown"
        "lang-php"
        "lang-terraform"
        "lang-web"
        "lang-yaml"
        "lang-lisp"
        "org"
        "prog-mode"
        "read-complete"
        "styling"
        "irc"
        "elfeed"
        "email"
        "floatpays"
        "project"
        "term"
        "development"
        "mastodon"
        ))



(let ((config-path (concat user-emacs-directory "modules/")))
  (dolist (module whk/modules)
    (condition-case err
        (load (concat config-path module ".el"))
      (error (display-warning module (error-message-string err))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
