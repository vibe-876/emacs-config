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

(server-start)

(use-package paredit
  :defer nil
  :hook ((scheme-mode     . enable-paredit-mode)
	   (emacs-lisp-mode . enable-paredit-mode)
	   (clojure-mode    . enable-paredit-mode)))

(use-package rainbow-delimiters
  :defer nil
  :hook ((scheme-mode     . rainbow-delimiters-mode)
	   (emacs-lisp-mode . rainbow-delimiters-mode)
	   (clojure-mode    . rainbow-delimiters-mode)))

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

(use-package cider
  :straight t
  :after (paredit rainbow-delimiters))

(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git
			  :host github
			  :repo "leanprover-community/lean4-mode"
			  :files ("*.el" "data"))
  :config
  (add-to-list 'exec-path (concat (getenv "HOME")
				    "/.elan/bin"))
  (setenv "PATH" (concat (concat (getenv "HOME") "/.elan/bin" ":")
			   (getenv "PATH"))))

(use-package rust-mode
  :config
  (let ((cargo-path)) (concat (getenv "HOME")
				"/cargo/bin")

	 (setenv "PATH" (concat (getenv "PATH")
				":" cargo-path))
	 (add-to-list 'exec-path cargo-path)))

(use-package rustic
  :hook (rustic-mode . lsp-mode)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package haskell-mode
  :hook ((haskell-mode          . interactive-haskell-mode)
	   (haskell-mode          . lsp)
	   (haskell-literate-mode . interactive-haskell-mode)
	   (haskell-literate-mode . lsp))
  :bind
  (:map haskell-mode-map ("C-c C-c" . haskell-compile))
  (:map haskell-cabal-mode-map ("C-c C-c" . haskell-compile))
  :config
  (let ((ghcup-path (concat (getenv "HOME") "/.ghcup/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" ghcup-path))
    (add-to-list 'exec-path ghcup-path)))

(use-package lsp-haskell
  :after (haskell-mode))

;; (use-package lsp-java
;;   :defer t
;;   :hook ((java-mode . lsp-mode)
;; 	   (lsp-mode  . lsp-java-mode))
;;   :after (:all lsp-mode magit)
;;   :config
;;   (setq lsp-enable-snippet nil))

(defun cam/java-run-in-buffer (&optional filename)
  "Just opens a buffer, and runs a Java program in it.
All it's doing is running java on the filename, and showing
the output in a buffer.

If FILENAME isn't given, then it will just run the current
buffer."
  (interactive)
  (let ((java-run-buffer-name "*cam/java-run*")
	  (java-buffer-name (if filename
				filename
			      (buffer-name))))

    (switch-to-buffer-other-window java-run-buffer-name)
    (erase-buffer)
    (start-process "*java-running-process*" java-run-buffer-name
		     "java" java-buffer-name)
    (other-window 1)))

(add-hook 'java-mode-hook
	    (lambda () (local-set-key (kbd "C-c C-r") #'cam/java-run-in-buffer)))

(use-package magit)

(use-package lsp-mode)

(use-package lsp-ui
  :after (lsp-mode))

(use-package company)

(require 'make-mode)
(define-key makefile-mode-map (kbd "C-o") (lambda nil
					    "Call casual-make."
					    (interactive)
					    (casual-make-tmenu)))

(defun cam/elfeed-remove-iplayer ()
  "I don't want iplayer or sounds from bbc,
just the articles. This marks them as read."
  (interactive)
  (let ((new-filter "@1-month-ago +unread +bbc !article"))

    (setq elfeed-search-filter new-filter)
    (elfeed-search-update :force)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)
    (elfeed-search-clear-filter)
    (message "BBC iplayer stuff removed.")))

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
	    ("https://feeds.bbci.co.uk/news/bbcindepth/rss.xml" news bbc)
	    ("https://www.quantamagazine.org/feed/" sci news)
	    ("https://feeds.libsyn.com/499093/rss" tech podcast)
	    ("http://hackaday.libsyn.com/rss" tech podcast)
	    ("https://rustacean-station.org/podcast.rss" tech prog podcast)
	    ("https://risky.biz/feeds/risky-business-news/" tech security podcast)
	    ("https://churchofturing.github.io/feed.xml" tech blog)
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

(setq erc-server "irc.libera.chat"
	etc-track-shorten-start 8
	erc-kill-buffer-on-part t
	erc-auto-query 'bury
	erc-user-full-name "cam a."
	erc-nick "vibe876")

(use-package emms
  :defer t
  :config
  (setq emms-player-list '(emms-player-mpv)
	  emms-source-file-default-directory "/home/cam/Music/music"))

(emms-all)

(setq org-directory (concat (getenv "HOME") "/Documents/Org")
	org-agenda-files (directory-files-recursively org-directory
						    (rx bol
							(one-or-more (or lower-case ?-))
							".org" eol)))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-directory (concat org-directory "/Agenda")
	org-agenda-work-file (concat org-agenda-directory "/work.org")
	org-agenda-social-file (concat org-agenda-directory "/social.org")
	org-agenda-personal-file (concat org-agenda-directory "/personal.org")

	org-capture-templates
	'(("u" "Uni Stuff" entry (file+headline org-agenda-work-file "Uni")
	   "* TODO [#C] %?\nSCHEDULED: %t")
	("s" "Social Stuff" entry (file+headline org-agenda-social-file "Misc")
	 "* TODO [#C] %?\nSCHEDULED: %t")
	("p" "Personal" entry (file+headline org-agenda-personal-file "Stuff ToDo")
	 "* TODO [#C] %?\nSCHEDULED: %t")))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-latex-listings 'minted
	org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
				"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
				"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
    
	org-html-validation-link nil
	org-export-with-author nil
	org-export-with-toc nil)

(use-package ef-themes
  :defer nil
  :config
  (load-theme 'ef-night t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;(defalias 'yes-or-no-p 'y-o-n-p)

(use-package casual-suite)

(use-package sequed
  :defer t)

(load-file (concat (getenv "HOME")
		     "/.emacs.d/secrets.el"))
