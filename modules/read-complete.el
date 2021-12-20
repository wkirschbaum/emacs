(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  (setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :init (savehist-mode))
(use-package recentf :init (recentf-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
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

;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))


(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


;; (defun my/xref-show-definitions-completing-read (fetcher alist)
;;   (let ((old-dir default-directory)
;;         (set-default-directory (lambda (dir) (setq default-directory dir))))
;;     (advice-add 'file-name-directory :filter-return set-default-directory)
;;     (unwind-protect
;;         (xref-show-definitions-completing-read fetcher alist)
;;       (setq default-directory old-dir)
;;       (advice-remove 'file-name-directory set-default-directory))))

(setq xref-show-xrefs-function 'xref-show-definitions-buffer)

;; (setq xref-show-xrefs-function 'xref-show-definitions-completing-read)




(setq xref-show-definitions-function 'consult-xref)
;; (setq xref-show-definitions-function 'xref-show-definitions-buffer)
