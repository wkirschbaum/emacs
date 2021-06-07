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

(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)
(setq default-frame-alist '((fullscreen . maximized)))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(setq-default indent-tabs-mode nil)
(setq-default auto-revert-verbose nil)
(setq-default wdired-allow-to-change-permissions t)
(setq-default wdired-create-parent-directories t)

(global-display-line-numbers-mode)
;; This goes into elixir-mode for some reason?
;; (global-subword-mode 1)

;;; native-config.el ends here
