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

    ;; escape character
    ;; (modify-syntax-entry ?# "\\" table)

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
             ("defp" exp "do" insts "end")
             ("defmodule" exp "do" insts "end")
             ("defprotocol" exp "do" insts "end")
             ("defmacrop" exp "do" insts "end")
             ("defmacro" exp "do" insts "end")
             ("quote" "do" insts "end")
             ("case" exp "do" insts "end")
             ("fn" matches "end")
             ("if" exp "do" insts "end")
             ("if" exp "do" insts "else" insts "end")
             ("if" short-do-else)
             ("try" "do" insts "rescue" matches "end")
             ("try" "do" insts "after" matches "end")
             ("try" "do" insts "catch" matches "end")
             ("try" "do" insts "end")
             ("with" exp "do" insts "end")
             ("with" exp "do" insts "else" matches "end")
             ("with" short-do-else))
       (insts (inst) (insts ";" insts))
       (short-do-else
        (exps "," "do:" exp)
        (short-do-else "," "else:" exp))
       (match (exp "->" insts))
       (matches (match) (matches "__stab_op_break__"  matches))
       (exp (exp "=" exp))
       (exps (exp) (exps "," exps))
       )
     '((assoc "__stab_op_break__"))
     '((assoc ";"))
     '((assoc ","))
     '((right "="))
     ))))

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
    ('(:elem . basic) elixir-indent-level)
    (`(:before . ,(or "(" "[" "{")) (smie-rule-parent))
    (`(:before . "->") elixir-indent-level)
    (`(:before . ,(or";" "__stab_op_break__"))
     (cond ((smie-rule-parent-p "do" "rescue" "[" "{" "(")
            (smie-rule-parent elixir-indent-level))
           (t (smie-rule-parent))))
    (`(:before . "=") elixir-indent-level)))

(defun whk/elixir-special-delim-p (token)
  (or
   (equal token ";")
   (equal token "__stab_op_break__")))

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
             (memq (char-before) '(?, ?= ?+ ?- ?* ?/))))))

(defun elixir-smie--stab-op-eol-p ()
  "Return t if the line contains a stab line without an fn initiator"
  (save-excursion
    (goto-char (line-end-position))
    (and (eq (char-before) ?>)
         (member (smie-default-backward-token) '("->"))
         (not (looking-back ".*fn[ \t].*"
                        (line-beginning-position))))))

(defun elixir-smie--stab-op-not-inline-p ()
  "Return t if the line contains a stab line without an fn initiator"
  (save-excursion
    (goto-char (line-end-position))
    (not (and (eq (char-before) ?>)
         (member (smie-default-backward-token) '("->"))))))

(defun elixir-smie--forward-token ()
  (skip-chars-forward " \t")
  (cond
   ((and
     (not (eobp))
     (looking-at "[\n#]")
     (elixir-smie--implicit-semi-p))
    (if (eolp) (forward-char 1))
    (forward-comment (point-max))
    (if (elixir-smie--stab-op-eol-p) "__stab_op_break__" ";"))
   (t (let ((token (smie-default-forward-token)))
        ;; if token is not eol stab then treat it as a operator
        ;; for inline indentation
        (if (and (equal token "->") (elixir-smie--stab-op-not-inline-p))
            "."
          token)))))

(defun elixir-smie--backward-token ()
  (let ((pos (point)))
    ;; Be careful, as the cond order matters below
    (cond
     ((progn
        (skip-chars-backward " \t")
        (and (bolp) (elixir-smie--stab-op-eol-p)))
      (forward-comment (- (point)))
      "__stab_op_break__")
     ((progn
        (forward-comment (- (point)))
        (and (> pos (line-end-position))
           (elixir-smie--implicit-semi-p)))
      ";")
     (t (let ((token (smie-default-backward-token)))
        ;; if token is not eol stab then treat it as a operator
        ;; for inline indentation
        (if (and (equal token "->") (elixir-smie--stab-op-not-inline-p))
            "."
          token))))))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  :syntax-table elixir-mode-syntax-table

  (smie-setup elixir-smie-grammar #'elixir-smie-rules
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
