;; ;;; package --- elixir-mode -*- lexical-binding: t; -*-
;; ;;; Commentary:
;; ;;; Code:

(require 'eglot)

;;(defvar whk/lsp-mode-enabled nil)
;; (defvar whk/eglot-enabled nil)
;; (load "~/src/emacs/heex-ts-mode/heex-ts-mode.el")
;; (load "~/src/emacs/elixir-ts-mode/elixir-ts-mode.el")

(defvar whk/eglot-enabled t)
(defvar whk/lsp-mode-enabled nil)
(use-package elixir-ts-mode
  :ensure t
  :config
  (global-subword-mode t))

(setq whk/elixir-lsp-sh "~/src/elixir/elixir-ls/bin/language_server.sh")

(dolist (mode '(elixir-mode elixir-ts-mode heex-ts-mode))
    (add-to-list 'eglot-server-programs `(,mode . (,whk/elixir-lsp-sh))))

(when whk/eglot-enabled
  (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
  (add-hook 'heex-ts-mode-hook 'eglot-ensure))

(when whk/lsp-mode-enabled
  (use-package lsp-mode
    :ensure t
    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
    :hook ((elixir-ts-mode . lsp)
           (heex-ts-mode   . lsp)
           (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp))

(defun elixir-ts-mode--clean-error-filename (filename)
  (message "|%s|" filename)
  (string-trim filename))

(define-compilation-mode elixir-ts-compilation-mode "Elixir Compilation Mode"
  "Compilation mode for Elixir mix command output."
  (setq compilation-scroll-output t)
  (setq-local compilation-parse-errors-filename-function
              #'elixir-ts-mode--clean-error-filename))

(defvar elixir-ts-mode--mix-previous-test-command
  "Last elixir mix test command ran." nil)

(defun elixir-ts-mode-mix-command (command)
  "Run elixir mix command specified by COMMAND."
  (let* ((default-directory (project-root (project-current)))
         (compilation-parse-errors-filename-function
          #'elixir-ts-mode--error-filename))
    (setq compilation-parse-errors-filename-function #'elixir-ts-mode--error-filename)
    (compile (concat "mix" " " command) 'elixir-ts-compilation-mode)))

(defun elixir-ts-mode-mix-test-command (&optional stale-p file-name-p line-num-p)
  "Run elixir mix command specified by COMMAND."
  (let* ((default-directory (project-root (project-current)))
         (file-relative (file-relative-name buffer-file-name))
         (line-num (number-to-string (line-number-at-pos)))
         (args (if stale-p " --stale" ""))
         (command
          (cond
           ((and file-name-p line-num-p) (concat "test " file-relative ":" line-num))
           (file-name-p (concat "test " file-relative))
           (t "test"))))
    (setq elixir-ts-mode--mix-previous-test-command command)
    (elixir-ts-mode-mix-command (concat command args))))

(defun elixir-ts-mode-run-test-rerun (&optional arg)
  "Run elixir test from the point of the cursor."
  (interactive "p")
  (when elixir-ts-mode--mix-previous-test-command
    (if (and (numberp arg) (> arg 1))
        (elixir-ts-mode-mix-command
         (concat elixir-ts-mode--mix-previous-test-command " --stale"))
      (elixir-ts-mode-mix-command
       elixir-ts-mode--mix-previous-test-command))))

(defun elixir-ts-mode-run-test-single (arg)
  (interactive "p")
  "Run elixir test from the point of the cursor."
  (interactive)
  (elixir-ts-mode-mix-test-command (and (numberp arg) (> arg 1)) t t))

(defun elixir-ts-mode-run-test-buffer (arg)
  "Run Elixir test from the buffer."
  (interactive "p")
  (elixir-ts-mode-mix-test-command (and (numberp arg) (> arg 1)) t))

(defun elixir-ts-mode-run-test-project (arg)
  "Run Elixir test from the project."
  (interactive "p")
  (elixir-ts-mode-mix-test-command (and (numberp arg) (> arg 1))))

(use-package mix :ensure t)
(use-package exunit
  :ensure t
  :bind
  ("C-c , a" . elixir-ts-mode-run-test-project)
  ("C-c , s" . elixir-ts-mode-run-test-single)
  ("C-c , v" . elixir-ts-mode-run-test-buffer)
  ("C-c , r" . elixir-ts-mode-run-test-rerun)
  ("C-c , t" . exunit-toggle-file-and-test))
