;; (use-package znc
;;   :ensure t)

(use-package erc
  :commands erc
  :config
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t)

  (setq erc-prompt-for-password nil)
  (setq erc-prompt-for-nickserv-password nil)

  (require 'erc-match)
  (setq erc-keywords '("floatpays"))

  (setq erc-log-channels-directory "~/.erc/logs/")
  (setq erc-save-buffer-on-part nil
        erc-save-queries-on-quit nil
        erc-log-write-after-send t
        erc-log-write-after-insert t)

  (setq erc-current-nick-highlight-type 'all)

  (require 'erc-fill)
  (erc-fill-mode)
  (setq erc-fill-column 81)

  ;; (require 'erc-netsplit)
  ;; (erc-netsplit-mode t)

  (erc-timestamp-mode t)
  (setq erc-timestamp-format "[%R-%m/%d]")

  (erc-button-mode nil) ;slow

  ;; Remove server part from modeline
  (setq erc-mode-line-format "%t")

  (setq erc-max-buffer-size 10000)
  (setq erc-hide-timestamps nil)
  (setq erc-rename-buffers t)
  (erc-spelling-mode t)

  (setq erc-rename-buffers t
        erc-lurker-hide-list '("JOIN" "QUIT")
        erc-lurker-threshold-time 3600))


(load "~/Cloud/secrets/irc-servers.el")
