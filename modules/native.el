;; Package --- native configuration
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(show-paren-mode 1)
(column-number-mode 1)
(set-default 'cursor-in-non-selected-windows 'hollow)

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

(which-function-mode t)

(save-place-mode 1) ;; Remember where I was last time I visited the file
(winner-mode 1)
;; (pixel-scroll-precision-mode 1)

(repeat-mode 1)

;; in case we want transparent backgrounds
;; adding this for future reference
;; (set-frame-parameter nil 'alpha-background 100)
;; (add-to-list 'default-frame-alist '(alpha-background . 100))

;; nicer scroll with mouse, is this better? not sure
(pixel-scroll-precision-mode)

(setq use-short-answers t)

(defun whk/set-line-numbers ()
  "Enable line numbers mode."
  (setq display-line-numbers-type 'relative)
  (setq display-line-numbers-current-absolute t)
  (setq-default display-line-numbers-width 4)
  (setq-default display-line-numbers-widen t)
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook 'whk/set-line-numbers)
(add-hook 'yaml-mode-hook 'whk/set-line-numbers)


(setq default-frame-alist '((fullscreen . maximized)))

(setq create-lockfiles nil)

;; apropos
(setq apropos-sort-by-scores t)

;; if we make backups put it into the backup directory
;; (setq make-backup-files nil)
;; (setq auto-save-default nil)
(setq auto-save-no-message t)

(make-directory "~/.config/emacs/backup/" t)
(setq auto-save-file-name-transforms '(("~/.config/emacs/backup/" t)))
(setq backup-directory-alist '(("." . "~/.config/emacs/backup/")))

(setq-default indent-tabs-mode nil)
(setq-default auto-revert-verbose nil)
(setq-default wdired-allow-to-change-permissions t)
(setq-default wdired-create-parent-directories t)

(setq sentence-end-double-space nil)

(global-set-key (kbd "C-x w a") 'windmove-left)
(global-set-key (kbd "C-x w e") 'windmove-right)
(global-set-key (kbd "C-x w p") 'windmove-up)
(global-set-key (kbd "C-x w n") 'windmove-down)

(global-set-key (kbd "C-h a") 'apropos-library)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key (kbd "C-c C-a") 'avy-goto-char)

(tab-bar-mode)
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-tab-hints t)

;; No need to sleep graphic display
(if (display-graphic-p)
    (progn
      (global-unset-key (kbd "C-z"))
      (global-unset-key (kbd "C-x C-z"))))

;; Show error as flashing modeline
(defun flash-mode-line ()
  "Flashes the mode-line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil)
(setq ring-bell-function 'flash-mode-line)

(provide 'native)
;;; native.el ends here

