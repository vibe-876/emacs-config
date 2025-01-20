(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-ensure nil)

(defun display-startup-echo-area-message nil
  "I don't want gnu's stuff at startup, I want my stuff."
  (message "Hello Cam :) ."))

(setq inhibit-startup-screen t)

(use-package paredit
  :defer nil
  :hook ((scheme-mode     . enable-paredit-mode)
	 (emacs-lisp-mode . enable-paredit-mode)))

(use-package rainbow-delimiters
  :defer nil
  :hook ((scheme-mode     . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package geiser
  :straight t
  :defer nil
  :config
  (setq geiser-active-implementations '(guile)))

(use-package geiser-guile
  :straight t
  :defer t
  :after (geiser)
  :config
  (setq geiser-guile-binary "/usr/bin/guile"))

(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git
			:host github
			:repo "leanprover-community/lean4-mode"
			:files ("*.el" "data"))
  :config
  (add-to-list 'exec-path "/home/cam/.elan/bin"))

(use-package magit)

(use-package lsp-mode)

(use-package elfeed
  :defer t
  :bind ("C-c e" . elfeed)
  :config
  (setq elfeed-feeds
	'(("https://planet.emacslife.com/atom.xml" blog emacs)
	  ("https://summeremacs.github.io/posts/index.xml" blog emacs)
	  ("https://xkcd.com/rss.xml" comic)
	  ("https://www.smbc-comics.com/comic/rss" comic)
	  ("https://www.monkeyuser.com/index.xml" comic)
	  ("https://archlinux.org/feeds/news/" arch linux tech)
	  ("https://wolfgirl.dev/blog/rss.xml" blog tech prog)
	  ("https://izzys.casa/index.xml" blog tech prog)
	  ("https://faultlore.com/blah/rss.xml" blog tech prog)
	  ("https://welltypedwit.ch/rss.xml" tech blog)
	  ("https://www.quantamagazine.org/feed/" sci news)
	  ("https://feeds.libsyn.com/499093/rss" tech podcast)
	  ("http://hackaday.libsyn.com/rss" tech podcast)
	  ("https://rustacean-station.org/podcast.rss" tech prog podcast)
	  ("https://risky.biz/feeds/risky-business-news/" tech security podcast)
	  ;; Leadhead
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3_kehZbfRz-KrjXIqeIiPw" blog video)
	  ;; Helluva Boss
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCzfyYtgvkx5mLy8nlLlayYg" video show)
	  ;; oliSUNvia
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVHxJghKAB_kA_5LMM8MD3w" video phil)
	  ;; Wendigoon
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3cpN6gcJQqcCM6mxRUo_dA" video spooky)
	  ;; ABSTRACT
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCIPfjC8FVLdul4-35JekB1g" video spooky)
	  ;; SOG
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtMVHI3AJD4Qk4hcbZnI9ZQ" video blog))))

(use-package emms
  :defer t
  :config
  (setq emms-player-list '(emms-player-mpv)
	emms-source-file-default-directory "/home/cam/Music/music"))

(use-package ef-themes
  :defer nil
  :config
  (load-theme 'ef-cherie t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defalias 'yes-or-no-p 'y-o-n-p)
