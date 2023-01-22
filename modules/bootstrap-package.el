;;; Package --- Bootstrapping  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun install-use-package ()
  (unless (package-installed-p 'use-package)
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
  (eval-when-compile (require 'use-package)))

(install-use-package)

(eval-when-compile
  (require 'use-package))



