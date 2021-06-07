(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '(("https://dashbit.co/feed" elixir blog)
          ("https://planet.emacslife.com/atom.xml" emacs blog)
          ("https://sachachua.com/blog/category/emacs-news/feed" emacs news)
          ("https://emacsair.me/feed.xml" emacs blog)
          ("https://mikrotik.com/download.rss" mikrotik news))))
