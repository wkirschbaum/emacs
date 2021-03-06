(use-package elfeed
  :ensure t
  :config
  (setq elfeed-sort-order "ascending")
  (setq elfeed-feeds
        '(("https://www.cognitive-edge.com/?feed=rss" blog)
          ("https://dashbit.co/feed" elixir blog)
          ("https://planet.emacslife.com/atom.xml" emacs blog)
          ("https://emacsair.me/feed.xml" emacs blog)
          ("https://mikrotik.com/download.rss" mikrotik news))))
