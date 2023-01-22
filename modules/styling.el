;; (use-package ef-themes
;;   :ensure t)


(load-theme 'modus-vivendi)


(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((regular
           :default-family "Hack"
           :default-height 120
           :fixed-pitch-family "Fira Code"
           :variable-pitch-family "Noto Sans"
           :italic-family "Source Code Pro"
           :line-spacing 1)
          (large
           :default-family "Hack"
           :default-height 170
           :line-spacing 1)
          (martin
           :default-family "Helvetica"
           :default-height 170
           :line-spacing 1)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Hack"
           :default-weight regular
           :default-height 120
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

        (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
        (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
        (define-key global-map (kbd "C-c f f") #'fontaine-set-preset))
