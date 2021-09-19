(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-idle-delay 0.3
        company-dabbrev-downcase nil
        )
  (global-company-mode))

(with-eval-after-load 'company
 (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
 (define-key company-active-map (kbd "<backtab>") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package company-restclient
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

(use-package company-terraform
  :ensure company
  :config
  (company-terraform-init))


(use-package company-ledger
  :ensure company
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ledger)))
