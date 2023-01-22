(use-package geiser-guile
  :ensure t)

(use-package macrostep-geiser
  :ensure t
  :after geiser-mode
  :config (add-hook 'geiser-mode-hook #'macrostep-geiser-setup))

(use-package macrostep-geiser
  :ensure t
  :after geiser-repl
  :config (add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup))

(require 'eldoc)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;; (use-package paredit
;;   :ensure t
;;   :autoload enable-paredit-mode
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook #'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook #'enable-paredit-mode)
;;   (add-hook 'geiser-mode-hook  #'enable-paredit-mode)
;;   ;; (add-hook 'geiser-repl-mode-hook  #'enable-paredit-mode)
;;   :config
;;   (show-paren-mode t))

(use-package macrostep
  :ensure t
  :bind ("C-c e" . macrostep-mode))
