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
    (modify-syntax-entry ?= "." table)
    ;; (modify-syntax-entry ?/ "." table)
    ;; (modify-syntax-entry ?+ "." table)
    ;; (modify-syntax-entry ?* "." table)
    ;; (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?: "'" table)

    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Strings
    ;; (modify-syntax-entry ?\' "\"'" table)
    ;; (modify-syntax-entry ?\" "\"\"" table)

    ;; Symbol constituents
    ;; (modify-syntax-entry ?! "_" table)
    ;; (modify-syntax-entry ?? "_" table)
    ;; (modify-syntax-entry ?_ "_" table)
    ;; (modify-syntax-entry ?@ "_" table)

    ;; expressions
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; escape character
    ;; (modify-syntax-entry ?# "\\" table)

    (modify-syntax-entry ?% "'" table)

    table)
  "Syntax table to use in Elixir mode.")

(defun whk/elixir-forward-token ()
  (interactive)
  (message (elixir-smie--forward-token)))

(defun whk/elixir-backward-token ()
  (interactive)
  (message (elixir-smie--backward-token)))

(defvar elixir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (lines (line) (lines ";" lines))
       (line (exp) ("def-start" def-body "end"))
       (line (exp) ("fn" def-body "end"))
       (def-body (exp "do" lines) (exp "->" lines))
       (exp (id)))

     '((assoc ";") (assoc "=")))
    (smie-precs->prec2
     '()))))

(defun elixir-smie--in-list-p ()
  (nth 1 (syntax-ppss (point))))

(defun elixir-smie--print-token (kind token)
  (message "(%s) %s %s" smie--parent kind token))

(defun elixir-smie-rules (kind token)
  "Elixir indentation rules for KIND and TOKEN."
  (progn
    ;; (elixir-smie--print-token kind token)
    (pcase (cons kind token)
      ('(:elem . basic) 0)
      ('(:after . ",") (if (elixir-smie--in-list-p) nil (smie-rule-parent 2)))
      (`(:before . ,(or "=" "do" "->")) elixir-indent-level)
      ('(:after . ";") 0))))

;; TODO: backward token checks can be optimised, but keeping it simple for now
(defun elixir-smie--implicit-semi-p ()
  (save-excursion
    (skip-chars-backward " \t")
    (and (eolp)
         (not (memq (char-before)
                    '(?\; ?- ?+ ?* ?/ ?. ?, ?\\ ?& ?> ?< ?% ?~ ?^ ?= ??)))
         (not (member (smie-default-backward-token) '("do" "->"))))))

(defun elixir-smie--search-token-on-line-p (&rest tokens)
  "Return t if TOKENS is found on the same line by skipping sexps."
  (save-excursion
    (move-end-of-line 1)
    (and (not (bolp)) (member (smie-default-backward-token) tokens))
    ))

(defun elixir-smie--forward-token ()
  "Elixir forward token."
  (skip-chars-forward " \t")
  (cond
   ((and (eolp)
         (not (bolp))
         (elixir-smie--implicit-semi-p))
    (progn (forward-char 1)
           (forward-comment (point-max))
           ";"))
   ((save-excursion
      (forward-comment (point-max))
      (and (smie-rule-bolp)
           (elixir-smie--search-token-on-line-p "do")))
    (progn
      (smie-default-forward-token)
      "def-start"))
   (t (smie-default-forward-token))))


(defun elixir-smie--backward-token ()
  "Elixir backward  token."
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (elixir-smie--implicit-semi-p))

      ";")
   ((save-excursion
      (smie-default-backward-token)
      (and (smie-rule-bolp)
           (elixir-smie--search-token-on-line-p "do")))
    (progn
      (smie-default-backward-token)
      "def-start"))
     (t (smie-default-backward-token)))))


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
