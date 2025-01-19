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

(defun display-startup-echo-area-message nil
  "I don't want gnu's stuff at startup, I want my stuff."
  (message "Hello Cam :) ."))

(setq inhibit-startup-screen t)

(straight-use-package 'paredit)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(straight-use-package 'magit)

(straight-use-package 'elfeed)
(global-set-key (kbd "C-c e") 'elfeed)

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
	("https://feeds.libsyn.com/499093/rss" tech podcast)
	("http://hackaday.libsyn.com/rss" tech podcast)
	("https://rustacean-station.org/podcast.rss" tech prog podcast)
	("https://risky.biz/feeds/risky-business-news/" tech security podcast)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UC3_kehZbfRz-KrjXIqeIiPw" blog video) ;; Leadhead
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCzfyYtgvkx5mLy8nlLlayYg" video show) ;; Helluva Boss
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCVHxJghKAB_kA_5LMM8MD3w" video phil) ;; oliSUNvia
	("https://www.youtube.com/feeds/videos.xml?channel_id=UC3cpN6gcJQqcCM6mxRUo_dA" video spooky) ;; Wendigoon
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCIPfjC8FVLdul4-35JekB1g" video spooky) ;; ABSTRACT
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCtMVHI3AJD4Qk4hcbZnI9ZQ" video blog) ;; SOG
	))

(straight-use-package 'ef-themes)
(load-theme 'ef-cherie t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defalias 'yes-or-no-p 'y-o-n-p)
