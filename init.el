;; Package --- Personal init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(progn
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (if (file-exists-p custom-file)
      (load custom-file)))

(defvar whk/modules
      '(
        "native"
        "minimal"
        "bootstrap"
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
        "floatpays"
        "project"
        "term"
        "development"
        "mastodon"
        ))

(defvar whk/private-modules
      '("sourcehut" "irc-servers" "email" "elfeed-feeds"))


(let ((config-path (concat user-emacs-directory "modules/")))
  (dolist (module whk/modules)
    (condition-case err
        (load (concat config-path module ".el"))
      (error (display-warning module (error-message-string err))))))

(let ((config-path (concat "~/Cloud/emacs/" "modules/")))
  (dolist (module whk/private-modules)
    (condition-case err
        (load (expand-file-name (concat config-path module ".el")))
      (error (display-warning module (error-message-string err))))))

(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
