(setq auth-sources '((:source "~/Cloud/secrets/.authinfo.gpg")))

(defun setup-custom-config (config-path)
  (setq custom-file (concat config-path "custom.el"))
  (if (file-exists-p custom-file)
      (load custom-file)))

