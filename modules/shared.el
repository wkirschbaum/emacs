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
  :demand t
  :bind ("C-x C-z" . selectrum-repeat)
  :config
  (setq precient-filter-method '(literal-prefix regexp initialism))
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

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


(use-package consult
  :straight t
  :demand t
  :bind ("M-i" . consult-imenu)
  :bind ("C-s" . consult-isearch)
  :bind ("M-g g" . consult-goto-line)
  :bind ("C-x b" . consult-buffer)
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
   :map minibuffer-local-map
   ("C-c C-o" . embark-occur))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (emark-collect-mode . emabark-consult-preview-minor-mode))

;; Project management

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
