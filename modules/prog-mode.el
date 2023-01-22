(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tab-mark trailing)))

(defun whk/whitespace-cleanup ()
  "Cleans white spaces in progmode."
  (when (derived-mode-p 'prog-mode)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'whk/whitespace-cleanup)

(use-package avy :ensure t)

(global-set-key (kbd "C-x , n") 'flymake-goto-next-error)
(global-set-key (kbd "C-x , p") 'flymake-goto-prev-error)
