;; -*- lexical-binding: t -*-

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "Url encode the region between START and END in the buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "Url decode the region between START and END in the buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun whk/get-url-content (url)
  "Make a get request to the URL and return the body."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (prog2
        (let ((end-of-header (progn
                             (goto-char (point-min))
                             (re-search-forward "^$")
                             (point))))
             (delete-region (point-min) end-of-header))
        (buffer-string)
      (kill-buffer))))

(defun whk/write-url-content ()
  "Yank the content of a URL in the buffer."
  (interactive)
  (insert
   (whk/get-url-content (read-string "url: "))))

(defun whk/random_text (length)
  (interactive "nlength: ")
  (dotimes (_ length)
    (insert (whk/random-alnum))))

(defun whk/random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789  ")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun whk/magit-find-file-dwim ()
  (interactive)
  (magit-find-file-other-window (magit-get-previous-branch) (buffer-file-name)))




(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is in this format
 x.html~2018-05-15_133429~
 The last part is hour, minutes, seconds.
in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done.

URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2018-05-15"
  (interactive)
  (let (($fname (buffer-file-name))
        ($date-time-format "%Y-%m-%d_%H%M%S"))
    (if $fname
        (let (($backup-name
               (concat $fname "~" (format-time-string $date-time-format) "~")))
          (copy-file $fname $backup-name t)
          (message (concat "Backup saved at: " $backup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda ($x)
                    (let (($backup-name
                           (concat $x "~" (format-time-string $date-time-format) "~")))
                      (copy-file $x $backup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "buffer not file nor dired")))))

(defun xah-make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(global-set-key (kbd "C-c b") 'xah-make-backup-and-save)

(provide 'functions)
;;; functions.el ends here
