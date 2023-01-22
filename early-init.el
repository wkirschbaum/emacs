;;; Package --- Early init optimisations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold (* 1024 1024 1024))

;; When idle for 10 sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 20 t
                       (lambda ()
                         (k-time (garbage-collect)))))

;; Disable package when enabling straight
(setq package-enable-at-startup nil)

(setq default-frame-alist '((fullscreen . maximized)))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; (set-face-attribute 'default nil  :family "Source Code Pro" :height 100 :weight 'normal)
(set-face-attribute 'default nil  :family "Hack" :height 120 :weight 'normal)

(provide 'early-init)
;;; early-init.el ends here
