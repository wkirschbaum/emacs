;;; elixir-mode.el --- Major mode for editing Elixir files -*- lexical-binding: t -*-

;; Version: 1.0.0
;; Authors: Wilhelm H Kirschbaum
;; Keywords: languages elixir
;; package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Experimental

;;; Code:


(ignore-errors
  (unload-feature 'elixir-mode))

(require 'smie)

(defgroup elixir nil
  "Major mode for editing Elixir code."
  :prefix "elixir-"
  :group 'languages)

(defcustom elixir-indent-level 2
  "Indentation of Elixir statements."
  :type 'integer
  :safe 'integerp)

(defconst elixir-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"'" table)
    (modify-syntax-entry ?\" "\"\"" table)

    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst elixir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (inst ("def" insts "do" insts "end"))
       (insts (inst) (insts ";" insts)))
     '((assoc ";"))))))

(defun elixir-smie-rules (kind token)
  (pcase (cons kind token)
    ('(:elem . basic) elixir-indent-level)))

(defconst elixir-smie--doc-start-token "__doc_start__")
(defconst elixir-smie--stab-op-token "__stab_op__")

(defun whk/elixir-special-delim-p (token)
  (or
   (equal token ";")
   (equal token elixir-smie--doc-start-token)
   (equal token elixir-smie--stab-op-token)))

(defun whk/elixir-clear-tokens ()
  "Debug forward token"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((token (elixir-smie--forward-token)))
        (cond ((whk/elixir-special-delim-p token)
               (delete-char (- (length token))))
              ((equal token "")
               (forward-sexp)))))))

(defun whk/elixir-forward-token-populate ()
  "Debug forward token"
  (interactive)
  (while (not (eobp))
    (let ((token (elixir-smie--forward-token)))
      (if (whk/elixir-special-delim-p token)
          (insert token))
      (if (equal token "")
          (forward-sexp)))))

(defun whk/elixir-backward-token-populate ()
  "Debug forward token"
  (interactive)
  (while (not (bobp))
    (let ((token (elixir-smie--backward-token)))
      (if (whk/elixir-special-delim-p token)
          (save-excursion
            (insert token)))
      (if (equal token "")
          (backward-sexp)))))

(defun whk/elixir-forward-token ()
  "Debug backward token"
  (interactive)
  (let ((token (elixir-smie--forward-token)))
    (message "f: \"%s\"" token)))

(defun whk/elixir-backward-token ()
  "Debug backward token"
  (interactive)
  (let ((token (elixir-smie--backward-token)))
    (message "f: \"%s\"" token)))

(defun elixir-smie--forward-token ()
  (skip-chars-forward " \t")
  (cond ((and (not (eobp)) (looking-at "[\n#]"))
         (if (eolp) (forward-char 1))
         (forward-comment (point-max))
         (if (not (eobp)) ";"))
        (t (smie-default-forward-token))))

(defun elixir-smie--backward-token ()
  (skip-chars-backward " \t")
  (cond
   ((and (bolp) (not (bobp)))
    (backward-char 1)
    (forward-comment (- (point-max))) ";")
   (t (smie-default-backward-token))))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  :syntax-table elixir-mode-syntax-table

  ;; Comments
  (setq-local comment-use-syntax t)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  (smie-setup elixir-smie-grammar #'elixir-smie-rules
              :forward-token  #'elixir-smie--forward-token
              :backward-token #'elixir-smie--backward-token))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-mode.el ends here
