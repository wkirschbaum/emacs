;; Package --- native configuration
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(set-default 'cursor-in-non-selected-windows 'hollow)
(global-auto-revert-mode t) ;; Ensure Cloud files exists for this (org mode agenda)
(save-place-mode 1) ;; Remember where I was last time I visited the file
(winner-mode 1)
(pixel-scroll-precision-mode 1)

(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)
(setq default-frame-alist '((fullscreen . maximized)))

(setq create-lockfiles nil)

;; if we make backups put it into the backup directory
;; (setq make-backup-files nil)
;; (setq auto-save-default nil)
(make-directory "~/.config/emacs/backup/" t)
(setq auto-save-file-name-transforms '(("~/.config/emacs/backup/" t)))
(setq backup-directory-alist '(("." . "~/.config/emacs/backup/")))

(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(setq-default indent-tabs-mode nil)
(setq-default auto-revert-verbose nil)
(setq-default wdired-allow-to-change-permissions t)
(setq-default wdired-create-parent-directories t)

(setq sentence-end-double-space nil)

(global-display-line-numbers-mode)

(global-set-key (kbd "C-x w a") 'windmove-left)
(global-set-key (kbd "C-x w e") 'windmove-right)
(global-set-key (kbd "C-x w p") 'windmove-up)
(global-set-key (kbd "C-x w n") 'windmove-down)

(global-set-key (kbd "C-h a") 'apropos-library)

(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(provide 'native)
;;; native.el ends here
