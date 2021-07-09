(use-package weblorg
  :ensure t)

(require 'weblorg)

(defun whk/publish ()
  (interactive)

  (weblorg-route
   :name "posts"
   :input-pattern "./posts/*.org"
   :template "post.html"
   :output "./posts/{{ slug }}.html"
   :url "/posts/{{ slug }}.html")

  (weblorg-export))
