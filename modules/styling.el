(set-face-attribute 'default nil :family "hack")

(load-theme 'modus-vivendi)

(defconst original-font-face-height (face-attribute 'default :height))

(defun whk/font-large ()
  (interactive ())
  (set-face-attribute 'default nil :height 170))

(defun whk/font-normal ()
  (interactive ())
  (set-face-attribute 'default nil :height original-font-face-height))

(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
