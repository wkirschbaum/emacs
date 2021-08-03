;; -*- lexical-binding: t -*-

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




(provide 'functions)
;;; functions.el ends here


