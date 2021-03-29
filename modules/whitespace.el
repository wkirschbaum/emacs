(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tab-mark trailing empty)))

(defun whk/whitespace-cleanup ()
  "Cleans white spaces in progmode."
  (when (derived-mode-p 'prog-mode)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'whk/whitespace-cleanup)
