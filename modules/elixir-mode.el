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

(defvar elixir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?- "." table)

    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Strings
    (modify-syntax-entry ?\' "\"'" table)
    (modify-syntax-entry ?\" "\"\"" table)

    ;; Symbol constituents
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?: "_" table)

    ;; expressions
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; what to do with you? expression prefix?
    (modify-syntax-entry ?% "'" table)

    table)
  "Syntax table to use in Elixir mode.")


(defconst elixir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (inst (exp)
             ("def" exp "do" insts "end")
             ("defmodule" exp "do" insts "end")
             ("try" "do" insts "rescue" insts "end"))
       (insts (inst) (insts ";" insts))
       (exp (exp "=" exp))
       )
     '((assoc ";"))
     '((left "="))))))

(defun elixir-debug--smie-parent ()
  (if (boundp 'smie--parent)
      (nth 2 (smie-indent--parent))
      nil))

(defun elixir-debug--smie-rules (kind token)
  (let ((parent (elixir-debug--smie-parent))
        (result (elixir-smie-rules kind token)))
    (message "\"%s\": (%s %s) -> %s" parent kind token result)
    result))

(defun elixir-smie-rules (kind token)
  (pcase (cons kind token)
    ('(:elem . basic) 0)
    (`(:before . ";")
     (cond ((smie-rule-parent-p "do")
            (smie-rule-parent elixir-indent-level))
           (t (smie-rule-parent))))
    (`(:before . "=") elixir-indent-level)))

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

(defun elixir-smie--implicit-semi-p ()
  "Return t if an implicit semi colon should be added"
  (save-excursion
    (skip-chars-backward " \t")
    (not (or (bolp)
             (memq (char-before) '(?\[ ?\( ?\{))
             (memq (char-before) '(?, ?= ?+ ?- ?* ?/))
             ;; (and (eq (char-before) ?>)
             ;;      (member (save-excursion (elixir-smie--backward-token))
             ;;              '("->")))
             ))))

(defun elixir-smie--forward-token ()
  (skip-chars-forward " \t")
  (cond
   ;; ((and (not (eobp)) (looking-at "[\n#]"))
   ;;  (if (eolp) (forward-char 1))
   ;;  (forward-comment (point-max))
   ;;  ";")
   (t (smie-default-forward-token))))

(defun elixir-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ;; ((and (> pos (line-end-position)))
     ;;  (skip-chars-forward " \t") ";")
     (t (smie-default-backward-token)))))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  :syntax-table elixir-mode-syntax-table

  (smie-setup elixir-smie-grammar #'elixir-debug--smie-rules
              :forward-token  #'elixir-smie--forward-token
              :backward-token #'elixir-smie--backward-token)

  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-mode.el ends here
