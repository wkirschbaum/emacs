;; Package --- Shared  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(x pgtk))
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
    (exec-path-from-shell-copy-env "MYSQL_SOCKET")
    (exec-path-from-shell-copy-env "POSTGRES_SOCKET")
    (exec-path-from-shell-initialize)))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package ag
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package flyspell
  :ensure t
  :hook (prog-mode . flyspell-prog-mode)
  :hook (text-mode . flyspell-mode))

(use-package restclient
  :ensure t)
