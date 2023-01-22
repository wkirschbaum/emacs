;; don't prompt for compilation command
;; use prefix to change command
(setq compilation-read-command nil)
(setq compilation-always-kill t)

;; ensure colors renders correctly in compilation mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (add-hook 'eshell-preoutput-filter-functions
;;           'ansi-color-filter-apply)

;; This is slower than the above, but adds colour to eshell
 (add-hook 'eshell-preoutput-filter-functions
           'ansi-color-apply)
