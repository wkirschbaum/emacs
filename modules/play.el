(defcustom whk/play-before-hook nil
  "Hook to run before we play"
  :type 'hook)

(defun whk/play ()
  (interactive)
  (run-hooks 'whk/play-before-hook)
  (message "foo bar"))


(defun whk/hook ()
  (message "hook"))

(defun whk/advice ()
  (message "advice"))

(add-hook 'whk/play 'whk/hook)
(advice-add 'whk/play :before #'whk/advice)

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))
