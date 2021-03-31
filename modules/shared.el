;; Package --- Shared  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons
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

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (setq precient-filter-method '(literal-prefix regexp initialism))
  (setq magit-completing-read-function #'selectrum-completing-read)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package projectile
  :ensure t
  :after (selectrum)
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
  (marginalia-mode)
  :config
  (advice-add #'marginalia-cycle :after
              (lambda ()
                (when
                    (bound-and-true-p selectrum-mode)
                  (selectrum-exhibit 'keep-selected))))
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))


(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function #'selectrum-completing-read))

;; (use-package forge
;;   :ensure t)

(use-package diff-hl
  :ensure t
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-post-refresh . diff-hl-magit-pre-refresh))
  :config
  (global-diff-hl-mode))
