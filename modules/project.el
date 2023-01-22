;; (with-eval-after-load 'project
;;   ;; Only more recent versions of project.el have `project-prefix-map' and
;;   ;; `project-switch-commands', though project.el is available in Emacs 25.
;;   (when (and magit-bind-magit-project-status
;;              (boundp 'project-prefix-map)
;;              ;; Only modify if it hasn't already been modified.
;;              (equal project-switch-commands
;;                     (eval (car (get 'project-switch-commands 'standard-value))
;;                           t)))
;;     (define-key project-prefix-map "d" #'magit-project-status)
;;     (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))
