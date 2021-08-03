(use-package lsp-mode
  :ensure t
  :init
  (add-to-list `exec-path "~/src/tools/elixir-ls/bin/")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-elixir-fetch-deps t)
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 8000
        lsp-modeline-code-actions-enable t)
  (setq lsp-file-watch-ignored-directories
        '("[/\\\\]\\.git$"
          "[/\\\\]\\.elixir_ls$"
          "[/\\\\]_build$"
          "[/\\\\]deps"
          "[/\\\\]assets$"
          "[/\\\\]cover$"
          "[/\\\\]node_modules$"
          "[/\\\\]submodules$"))
  :hook (
         (elixir-mode . lsp)
         (typescript-mode . lsp)
         (dart-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-ui-doc-enable nil))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; optionally if you want to use debugger
;; (use-package dap-mode :ensure t)
;; (use-package dap-elixir)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (setq lsp-keymap-prefix "C-c l")
;;   (setq lsp-enable-file-watchers t
;;         lsp-file-watch-threshold 8000
;;         lsp-modeline-code-actions-enable t
;;         lsp-file-watch-ignored-directories
;;         '("[/\\\\]\\.git$"
;;           "[/\\\\]\\.elixir_ls$"
;;           "[/\\\\]_build$"
;;           "[/\\\\]deps"
;;           "[/\\\\]assets$"
;;           "[/\\\\]cover$"
;;           "[/\\\\]node_modules$"
;;           "[/\\\\]submodules$"
;;           ))
;;   (add-to-list `exec-path "~/src/tools/elixir-ls/bin/")
;;   :hook ((elixir-mode . lsp)
;;          (rust-mode . lsp))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil
;;         lsp-ui-sideline-show-hover nil
;;         lsp-ui-peek-enable nil))
