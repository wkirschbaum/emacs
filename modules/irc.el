;; (use-package znc
;;   :ensure t)

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
(setq erc-fill-column 81)
(setq erc-max-buffer-size 10000)
(setq erc-hide-timestamps nil)
(setq erc-rename-buffers t)
(erc-spelling-mode t)

(setq erc-rename-buffers t
      erc-lurker-hide-list '("JOIN" "QUIT")
      erc-lurker-threshold-time 3600)

(defun erc-connect ()
  (interactive)
  (erc-tls :server "znc.office.fltpay.net" :port "6680" :nick "whk/libera"))

(require 'erc-notify)
(setq erc-notify-list '("whk_alt" "josevalim"))
