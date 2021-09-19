(setq org-roam-v2-ack t)

(use-package org
  :config
  (setq org-agenda-files (file-expand-wildcards "~/Cloud/notes/*.org"))

  (setq-default org-todo-keywords
                '((sequence "TODO(t)" "DOING(b)" "|" "DONE(d)")))

  ;; When I am more comfortable with calendar
  ;; (setq-default org-agenda-include-diary t)

  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  (setq adaptive-fill-mode t)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'yaml-mode-hook 'turn-off-auto-fill)

  (setq org-log-done 'time)

  ;; Force a new line when the text goes too far to the right
  ;; it uses the default fill column number
  (add-hook 'org-mode-hook #'toggle-word-wrap)
  (add-hook 'text-mode-hook #'toggle-word-wrap)

  ;; Calendar and Diary
  (setq calendar-view-diary-initially-flag t
        calendar-mark-diary-entries-flag t
        european-calendar-style 't
        diary-file "~/Cloud/notes/diary")

  (setq org-catch-invisible-edits 'error))


(use-package ob-async
  :ensure t)

(use-package ob-elixir
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ruby . t)
   (elixir . t)
   (shell . t)
   ))

;; (defun mpv-play-url (url &rest args)
;;   ""
;;   (interactive)
;;   (start-process "mpv" nil "mpv" url))

;; (setq browse-url-handlers
;;       (quote
;;        (("youtu\\.?be" . mpv-play-url)
;;         ;; catch all
;;         ("." . browse-url-firefox-program))))

;; Persist org-clock between sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)

(setq org-ellipsis " ↴")

(use-package org-roam
  :ensure t
  :bind ("C-c C-j" . org-roam-dailies-find-today)
  :bind ("C-c M-c" . org-roam-capture)
  :bind ("C-c r f" . org-roam-node-find)
  :bind ("C-c r r" . org-roam)
  :bind ("C-c r i" . org-roam-insert)
  :config
  (add-hook 'after-init-hook 'org-roam-mode)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-directory "~/Cloud/notes"))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package org-contrib
  :ensure t)

(use-package ox-confluence
  :after org-contrib)

(provide 'org)
;;; org.el ends here
