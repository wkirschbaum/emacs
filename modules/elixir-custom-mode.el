;;; elixir-mode.el --- Major mode for editing Elixir files -*- lexical-binding: t -*-

;; Version: 1.0.0
;; Authors: Wilhelm H Kirschbaum
;; Keywords: languages elixir
;; package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Experimental Elixir mode with better handling of sexps

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

(defvar elixir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    ;; (modify-syntax-entry ?< "." table)
    ;; (modify-syntax-entry ?> "." table)
    ;; (modify-syntax-entry ?& "." table)
    ;; (modify-syntax-entry ?| "." table)
    ;; (modify-syntax-entry ?= "." table)
    ;; (modify-syntax-entry ?/ "." table)
    ;; (modify-syntax-entry ?+ "." table)
    ;; (modify-syntax-entry ?* "." table)
    ;; (modify-syntax-entry ?- "." table)
    ;; (modify-syntax-entry ?: "." table)

    ;; Comments
    ;; (modify-syntax-entry ?# "<" table)
    ;; (modify-syntax-entry ?\n ">" table)

    ;; Strings
    ;; (modify-syntax-entry ?\' "\"'" table)
    ;; (modify-syntax-entry ?\" "\"\"" table)

    ;; Symbol constituents
    ;; (modify-syntax-entry ?! "_" table)
    ;; (modify-syntax-entry ?? "_" table)
    ;; (modify-syntax-entry ?_ "_" table)
    ;; (modify-syntax-entry ?@ "_" table)

    ;; expressions
    ;; (modify-syntax-entry ?\( "()" table)
    ;; (modify-syntax-entry ?\) ")(" table)
    ;; (modify-syntax-entry ?\{ "(}" table)
    ;; (modify-syntax-entry ?\} "){" table)
    ;; (modify-syntax-entry ?\[ "(]" table)
    ;; (modify-syntax-entry ?\] ")[" table)

    ;; escape character
    ;; (modify-syntax-entry ?# "\\" table)

    (modify-syntax-entry ?% "'" table)

    table)
  "Syntax table to use in Elixir mode.")

(defvar elixir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (inst ("defmodule" id "do" exps "end"))
       (exps (exps ";" exps) (exp)))
     '((assoc ";"))))))

(defun elixir-smie-rules (kind token)
  "Elixir indentation rules for KIND and TOKEN."
  (progn
    (message "%s %s" kind token)
  (pcase (cons kind token)
    (`(:elem . basic) 2))))

(defvar elixir-keywords-regexp
  (regexp-opt '("+" "*" "," ";" ">" ">=" "<" "<=" "=")))

(defun elixir-smie-forward-token ()
  "Elixir forward token."
  (forward-comment (point-max))
  (cond
   ((looking-at "\"\"\"")
    (let* ((point (point)) (end (scan-sexps point 3)))
      (goto-char end)
      (buffer-substring-no-properties
       (1+ point) (1- end))))
   ((looking-at elixir-keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))))

(defun elixir-smie-backward-token ()
    "Elixir backward  token."
  (forward-comment (- (point)))
  (cond
   ((looking-back elixir-keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "w_")
              (point))))))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  :syntax-table elixir-mode-syntax-table

  (smie-setup elixir-smie-grammar #'elixir-smie-rules
              :forward-token  #'elixir-smie-forward-token
              :backward-token #'elixir-smie-backward-token)

  (setq-local comment-start "# ")
  (setq-local comment-end "")
  ;; (setq-local comment-start-skip "#+ *")
  ;; (setq-local parse-sexp-ignore-comments t)
  ;; (setq-local parse-sexp-lookup-properties t)
  ;; (setq-local paragraph-start (concat "$\\|" page-delimiter))
  ;; (setq-local paragraph-separate paragraph-start)
  ;; (setq-local paragraph-ignore-fill-prefix t)
  )


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-custom-mode.el ends here
