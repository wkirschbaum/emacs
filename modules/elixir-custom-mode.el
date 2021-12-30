;;; elixir-mode.el --- Major mode for editing Elixir files -*- lexical-binding: t -*-

;; Version: 1.0.0
;; Authors: wkirschbaum
;; Keywords: languages elixir
;; package-Requires: ((emacs "25.1"))

;; TODO

;; - comments
;; - docs
;; - """ comments
;; - sexps with @doc before ( to ignore )
;; - sigils
;; - start and end of defun levels
;; - when statements
;; - font-lock


;;; Code:

(ignore-errors
  (unload-feature 'elixir-mode))

(require 'smie)

(defgroup elixir-mode nil
  "Support for Elixir code"
  :link '(url-link "https://elixir-lang.org")
  :group 'languages)

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
             ("defmodule" exps "do" insts "end")
             ("defprotocol" exps "do" insts "end")
             ("def" exps "do" insts "end")
             ("try" "do" insts "after" matches "end")
             ("try" "do" insts "catch" matches "end")
             ("try" "do" insts "rescue" matches "end")
             ("try" "do" insts "end")
             ("if" exp "do" insts "end")
             ("if" exp "do" insts "else" insts "end")
             ("if" short-do-else)
             ("for" exps "do" insts "end")
             ("for" short-do-else)
             ("with" exps "do" insts "end")
             ("with" exps "do" insts "else" matches "end")
             ("with" short-do-else)
             ("fn" matches "end"))
       (insts (inst) (insts ";" insts))
       (match (exp "->" insts))
       (matches (match) (matches "stab_eol" matche))
       (short-do-else
        (exps "," "do:" exp)
        (short-do-else "," "else:" exp))
       (exps (exp) (exps "," exps))
       (exp (exp "=" exp)
            (exp "/" exp)
            (exp "*" exp)
            (exp "+" exp)
            (exp "-" exp)
            (exp "in" exp)
            ("when" exp)
            ("(" exp ")")))
     '((assoc ";"))
     '((assoc "stab_eol"))
     '((assoc ","))
     '((assoc "in")
       (assoc "when")
       (assoc "=")
       (assoc "*" "/")
       (assoc "+" "-"))))))

(defconst elixir-block-beg-keywords
  '("def" "defp" "defmodule" "defprotocol"
    "defmacro" "defmacrop" "defdelegate"
    "defexception" "defstruct" "defimpl"
    "defguard" "defguardp" "defcallback"
    "defoverridable"))

(defconst elixir-block-mid-keywords
  '("do" "else" "rescue" "catch"))

(defconst elixir-block-end-keywords
  '("end"))

(defconst elixir-block-beg-re
  (regexp-opt elixir-block-beg-keywords))

(defconst elixir-block-end-re
  (regexp-opt elixir-block-end-keywords))

(defconst elixir-block-keywords
  (append
   '(
     "fn" "case" "for" "in" "cond" "when"
     "if" "try" "raise" "do" "else" "true" "false" "with")
   elixir-block-beg-keywords
   elixir-block-end-keywords
   elixir-block-mid-keywords))

(defconst elixir-block-keywords-re
  (regexp-opt elixir-block-keywords))

(defun elixir-smie--implicit-semi-p ()
  "Return t if an implicit semi colon should be added"
  (save-excursion
    (skip-chars-backward " \t")
    (not (or (bolp)
             (memq (char-before) '(?\[ ?\( ?\{))
             (memq (char-before) '(?, ?= ?+ ?- ?* ?/))
             (and (eq (char-before) ?>)
                  (member (save-excursion (elixir-smie--backward-token))
                          '("->")))
             ))))

(defun elixir-smie--stab-eol-p ()
  "Return t if the next line is a stab_op"
  (save-excursion
    (skip-chars-forward " *\n")
    (and (not (looking-at "fn.*->.*" (line-end-position)))
         (looking-at ".*->.*" (line-end-position)))))

(defun elixir-smie--forward-token ()
  (skip-chars-forward " \t")
  (cond
   ((and
     (looking-at "[\n#]")
     (elixir-smie--implicit-semi-p))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ;; inject a stab_eol if the next line is a stab_op
    ;; so that we can add matches as a rule
    (if (elixir-smie--stab-eol-p) "stab_eol" ";"))
   (t (smie-default-forward-token))))

(defun elixir-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond ((and (> pos (line-end-position)) (elixir-smie--implicit-semi-p))
           (skip-chars-forward " \t") ";")
          (t
           (skip-chars-backward " \t")
           (smie-default-backward-token)))))

(defvar elixir-font-lock-keywords
  (append `((,(regexp-opt elixir-block-keywords 'symbols) . font-lock-keyword-face))))

(defun elixir-mode-syntactic-face-function (state)
  "Return face that distinguishes doc and normal comments in given syntax STATE."
  'font-lock-string-face
  (if (nth 3 state) 'font-lock-string-face 'font-lock-comment-face))

(defun elixir-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.
With ARG, move backward multiple defuns.  Negative ARG means
move forward."
  (interactive "p")
  (let ((count (or arg 1)))
    (if (< count 0)
        (elixir-end-of-defun (- count))
      (and (re-search-backward (concat "^\\s *" elixir-block-beg-re "\\_>") nil t count)
           (beginning-of-line)
           t
           ))))

(defun elixir-end-of-defun (&optional arg)
  "Move point to the end of the current defun.
The defun begins at or after the point.  This function is called
by `end-of-defun'."
  (interactive "p")
  (let ((count (or arg 1)))
    (if (< count 0)
        (elixir-beginning-of-defun (- count))
      (and (re-search-forward (concat "^\\s *" elixir-block-end-re "\\_>") nil t count)
           (end-of-line)
           t
           ))))

(defcustom elixir-indent-level 2
  "Indentation of Elixir statements."
  :type 'integer
  :safe 'integerp)

(defun elixir-smie-rules(kind token)
  (let
      ((result
        (pcase (cons kind token)
          ('(:elem . basic) elixir-indent-level)
          (`(:before . "->") elixir-indent-level)
          (`(:before . "stab_eol") elixir-indent-level)
          (`(:before . ";")
           (cond
            ((apply #'smie-rule-parent-p elixir-block-mid-keywords)
             (smie-rule-parent elixir-indent-level))))
          (`(:before . ,(or "=" "+" "-" "*" "/"))
           (cond
            ((smie-rule-parent-p nil) elixir-indent-level)
            (t (smie-rule-parent elixir-indent-level)))
           )
          )))
    (progn
      (message "(%s . (\"%s\") -> %s" kind token result)
      result)))

;;;###autoload
(define-derived-mode elixir-mode
  prog-mode "Elixir Custom"
  "Major mode for elixir."
  :group 'elixir-mode
  :syntax-table elixir-mode-syntax-table

  ;; (kill-all-local-variables)

  (setq-local smie-indent-basic elixir-indent-level)

  (smie-setup elixir-smie-grammar #'elixir-smie-rules
              :forward-token  #'elixir-smie--forward-token
              :backward-token #'elixir-smie--backward-token)


  (setq-local beginning-of-defun-function 'elixir-beginning-of-defun)
  (setq-local end-of-defun-function 'elixir-end-of-defun)

  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+\\s-*")

  ;; Syntax
  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)

  ;; Fonts
  (setq-local font-lock-defaults
              '(elixir-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . elixir-mode-syntactic-face-function)))
  )

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
