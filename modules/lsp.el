(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 8000
        lsp-modeline-code-actions-enable t
        lsp-file-watch-ignored-directories
        '("[/\\\\]\\.git$"
          "[/\\\\]\\.elixir_ls$"
          "[/\\\\]_build$"
          "[/\\\\]deps"
          "[/\\\\]assets$"
          "[/\\\\]cover$"
          "[/\\\\]node_modules$"
          "[/\\\\]submodules$"
          ))
  :hook ((elixir-mode . lsp)
         (rust-mode . lsp))
  :commands lsp
  :init
  (add-to-list `exec-path "~/src/tools/elixir-ls/bin/"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-peek-enable nil))
