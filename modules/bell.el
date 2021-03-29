(if (display-graphic-p)
    (progn
      (global-unset-key (kbd "C-z"))
      (global-unset-key (kbd "C-x C-z"))))

(defun flash-mode-line ()
  "Flashes the mode-line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil)
(setq ring-bell-function 'flash-mode-line)
