;; (setq load-path (cons  "~/.asdf/installs/erlang/24.1.7/lib/tools-3.5.1/emacs" load-path))
;; (setq erlang-root-dir "~/.asdf/installs/erlang/24.1.7")
;; (setq exec-path (cons "~/.asdf/installs/erlang/24.1.7/bin" exec-path))
;; (require 'erlang-start)


(use-package erlang
  :ensure t
  :config
  (require 'erlang-start))
