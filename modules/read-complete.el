(fido-mode)

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-idle-delay 0
;;         company-minimum-prefix-length 2
;;         company-idle-delay 0.3
;;         company-dabbrev-downcase nil
;;         )
;;   (global-company-mode))

;; (with-eval-after-load 'company
;;  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
;;  (define-key company-active-map (kbd "<backtab>") (lambda () (interactive) (company-complete-common-or-cycle -1))))

;; (with-eval-after-load 'company
;;   (define-key company-active-map (kbd "M-n") nil)
;;   (define-key company-active-map (kbd "M-p") nil)
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; (use-package company-restclient
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'company
;;     (add-to-list 'company-backends 'company-restclient)))

;; (use-package company-terraform
;;   :ensure t
;;   :config
;;   (company-terraform-init))


;; (use-package company-ledger
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'company
;;     (add-to-list 'company-backends 'company-ledger)))



;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Grow and shrink minibuffer
;;   (setq resize-mini-windows t)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   (setq read-extended-command-predicate
;;         #'command-completion-default-include-p)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t)

;;   ;; Ignores case when completing ( works with vertico )
;;   (setq read-file-name-completion-ignore-case t
;;         read-buffer-completion-ignore-case t
;;         completion-ignore-case t)

;;   ;; With corfu
;;   (setq completion-cycle-threshold 3)
;;   (setq tab-always-indent 'complete))


;; ;; Enable vertico
;; (use-package vertico
;;   :ensure t
;;   :init
;;   (vertico-mode)
;;   (setq vertico-cycle t))

;; ;; Use the `orderless' completion style.
;; ;; Enable `partial-completion' for files to allow path expansion.
;; ;; You may prefer to use `initials' instead of `partial-completion'.
;; (use-package orderless
;;   :ensure t
;;   :init
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; (use-package recentf :init (recentf-mode))

;; (use-package marginalia
;;   :ensure t
;;   :init
;;   (marginalia-mode))

;; (use-package embark
;;   :ensure t
;;   :demand t
;;   :bind
;;   (("C-." . embark-act)
;;    ("C-;" . embark-dwim)
;;    ("C-h B" . embark-bindings)
;;   )
;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; (use-package consult
;;   :ensure t
;;   :demand t
;;   :bind (("M-i" . consult-imenu)
;;          ("M-g g" . consult-goto-line)
;;          ("M-s r" . consult-ripgrep)
;;          ("C-x b" . consult-buffer))
;;   :init
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref
;;         xref-search-program 'ripgrep)
;;   :config
;;   (setq consult-project-root-function #'vc-root-dir))

;; ;; (use-package embark-consult
;; ;;   :ensure t
;; ;;   :after (embark consult)
;; ;;   :demand t
;; ;;   :hook
;; ;;   (embark-collect-mode . consult-preview-at-point-mode))


;; (use-package consult-dir
;;   :ensure t
;;   :bind (("C-x C-d" . consult-dir)
;;          :map vertico-map
;;          ("C-x C-d" . consult-dir)
;;          ("C-x C-j" . consult-dir-jump-file)))


;; (setq xref-show-xrefs-function 'xref-show-definitions-buffer)
;; (setq xref-show-definitions-function 'consult-xref)


;; (use-package corfu
;;   :ensure t
;;   :custom
;;   (corfu-auto t)
;;   (corfu-quit-no-match 'separator)
;;   :init
;;   (global-corfu-mode))


;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (setq-local corfu-auto nil)
;;             (corfu-mode)))

;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

