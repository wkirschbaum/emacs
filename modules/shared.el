;; Package --- Shared  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(x))
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
    (exec-path-from-shell-copy-env "MYSQL_SOCKET")
    (exec-path-from-shell-initialize)))

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

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flyspell
  :ensure t
  :hook (prog-mode . flyspell-prog-mode)
  :hook (text-mode . flyspell-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
)

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  ;;(setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; (use-package selectrum
;;   :ensure t
;;   :config
;;   (selectrum-mode +1))

;; (use-package selectrum-prescient
;;   :ensure t
;;   :config
;;   (setq precient-filter-method '(literal-prefix regexp initialism))
;;   (setq magit-completing-read-function #'selectrum-completing-read)
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))

(use-package projectile
  :ensure t
  :after (vertico)
  :demand t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-file-exists-remote-cache-expire nil
        projectile-completion-system 'default
        projectile-dynamic-mode-line t
        projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  (projectile-mode +1))

(use-package consult
  :straight t
  :demand t
  :bind (("M-i" . consult-imenu)
         ("M-g g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        xref-search-program 'ripgrep)
  :config
  (setq consult-project-root-function #'vc-root-dir))

(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-S-a" . embark-act)       ;; pick some comfortable binding
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
  )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Project management

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package magit
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package forge
  :ensure t)

(use-package diff-hl
  :ensure t
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-post-refresh . diff-hl-magit-pre-refresh))
  :config
  (global-diff-hl-mode))
