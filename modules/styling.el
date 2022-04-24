;; (use-package zeno-theme :ensure t :config (load-theme 'zeno))

;; (use-package modus-themes
;;   :ensure t
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   ;; (setq modus-themes-italic-constructs t
;;   ;;       modus-themes-bold-constructs nil
;;   ;;       modus-themes-region '(bg-only no-extend))

;;   ;; Load the theme files before enabling a theme (else you get an error).
;;   (modus-themes-load-themes)
;;   :config
;;   ;; Load the theme of your choice:
;;   (modus-themes-load-vivendi))

;; (load-theme 'tango-dark)

(set-face-attribute 'default nil :family "hack")

(load-theme 'wombat)

;; Wombat customization
(set-face-background 'default "#111")
(set-face-foreground 'font-lock-function-name-face "#95e454")
(set-face-foreground 'font-lock-string-face "#a9bc7a")

;; (set-face-attribute 'default nil :height 137)

(defconst original-font-face-height (face-attribute 'default :height))

(defun whk/font-large ()
  (interactive ())
  (set-face-attribute 'default nil :height 170))

(defun whk/font-normal ()
  (interactive ())
  (set-face-attribute 'default nil :height original-font-face-height))


;; (set-face-background 'lazy-highlight "#960")
;; (set-face-foreground 'lazy-highlight "#ccc")

(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
