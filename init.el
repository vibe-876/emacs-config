(setq gc-cons-threshold (* 50 1000 1000))

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


;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)

(setq straight-use-package-by-default t
      use-package-always-defer t)

(setq package-selected-packages '(lsp-mode treemacs focus yaml pkg-info auctex which-key dracula-theme elfeed emms eradio geiser geiser-guile geiser-chicken haskell-mode casual-agenda casual-avy casual-dired casual-info magit org-bullets org-ref org-roam org-superstar paredit poker rainbow-delimiters rustic ulisp-repl use-package ace-window avy academic-phrases arduino-mode 2048-game bui dap-mode casual 0blayout))

(defun cam/final-element (list)
  "Takes a list, and returns the final
element."
  (if (eq (cdr list) nil)
      (car list)
    (cam/final-element (cdr list))))

(defmacro cam/file-extension-regex (extension mode)
  "Creates a cons cell, where the car is a regex for
some given file extension, and the cdr is some mode.

This is meant to be used in the `auto-mode-alist'."
  `(cons (concat "\\." ,extension "$") ,mode))

(defmacro cam/default-value (value default)
  "Just a little macro to make optional arguments
to procedures a little easier to read."
  `(when (equal ,value nil) (setq ,value ,default)))

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(defun cam/load-init (&optional config-dir config-file output-file)
  "Tangle and reload ~/.emacs.d/init.org, because it is a pain to do it manually.

If a different file is used as a config, then arguments can be passed. CONFIG-DIR
is the directory that cam/load-init will assume the config files live in, CONFIG-FILE
is the literate org file that is to be tangled, and OUTPUT-FILE is the emacs lisp
file that will be tangled to, and then loaded."
  (interactive)
  (cam/default-value config-dir (concat (getenv "HOME") "/.emacs.d"))
  (cam/default-value config-file "/init.org")
  (cam/default-value output-file "/init.el")

  (save-buffer (concat config-dir config-file))
  (org-babel-tangle nil (concat config-dir config-file))
  (load-file (concat config-dir output-file))
  (message "Done :)"))

(global-set-key (kbd "M-p M-s") 'cam/load-init)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(global-set-key (kbd "C-c M-p") (lambda nil "Open ~/.emacs.d/init.org ."
  				(interactive)
  				(find-file (concat (getenv "HOME") "/.emacs.d/init.org"))))

(require 're-builder)
(setq reb-re-syntax 'rx)

(use-package geiser
  :straight t
  :defer nil
  :config
  (setq geiser-active-implementations '(guile))
  :hook ((scheme-mode . rainbow-delimiters-mode)
       (scheme-mode . enable-paredit-mode)))

(use-package geiser-guile
  :straight t
  :config
  (setq geiser-guile-binary "/usr/bin/guile"))

(use-package geiser-chicken
  :straight t
  :config
  (setq geiser-chicken-binary "/sbin/chicken-csi"))

(use-package haskell-mode
  :straight t
  :bind
  (:map haskell-mode-map ("C-c C-c" . haskell-compile))
  (:map haskell-cabal-mode-map ("C-c C-c" . haskell-compile))
  :config
  (let ((cam-ghcup-path (expand-file-name "~/.ghcup/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" cam-ghcup-path))
    (add-to-list 'exec-path cam-ghcup-path)))

(defun cam/haskell-load-literate-file nil
  "Load a literate org file containing Haskell code
into ghci.

I'll warn you now, this is a very messy implementation
of something that I have a feeling could be a lot more
simple."
  (interactive)
  (let ((file-name (buffer-file-name))
        (haskell-file (concat (file-name-sans-extension buffer-file-name) ".hs"))
        (cabal-file "../*.cabal")
        (process-name (concat "*"
                              (downcase
                               (file-name-sans-extension
                                (car
                                 (cdr (split-string (car (file-expand-wildcards "../*.cabal"))
                                                    "/")))))
                              "*")))

    (org-babel-tangle nil haskell-file)
    (find-file cabal-file t)
    (haskell-process-cabal-build)

    (find-file haskell-file)
    (haskell-process-load-file)
    (haskell-interactive-mode-clear)
    (kill-buffer (cam/final-element (split-string haskell-file "/")))

    (find-file file-name)
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer process-name)
    (other-window 1)
    (message "Done.")))

(use-package lean4-mode
  :straight (lean4-mode
             :type git
             :host github
             :repo "leanprover/lean4-mode"
             :files ("*.el" "data"))
  :commands (lean4-mode)
  :config
  (add-to-list 'exec-path (concat (getenv "HOME")
                                  ".elan/bin")))

(setenv "PATH" (concat (getenv "PATH") ":"
                         (concat (getenv "HOME")
                                 "/.elan/bin")))

(use-package rust-mode
  :straight t
  :config
  (setq cargo-path (concat (getenv "HOME")
                           "./cargo/bin"))
  (setenv "PATH" (concat (getenv "PATH")
                         ":"
                         cargo-path))
  (add-to-list 'exec-path cargo-path))

(use-package rustic
  :straight t
  :ensure t
  :config
  (setq rustic-format-on-save nil
        rustic-lsp-client 'lsp-mode)
  (let (rust-file-extension (cam/file-extension-regex "rs"))
    (add-to-list 'auto-mode-alist (cam/file-extension-regex "rs" 'rustic-mode)))

  :custom
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))

  :after (rust-mode))

(defun cam/arduino-compile-and-load (&optional board-name board-port path-to-root)
  "Just compiles and then loads an arduino sketch.
Meant to be used alongside `serial-term'.

Defaults to an arduino uno, because that's what I use.

If a serial connection is already open inside emacs for the
port that we want to connect to, it kills that buffer."
  (interactive)
  (cam/default-value board-name "arduino:avr:uno")
  (cam/default-value board-port "/dev/ttyACM0")
  (cam/default-value path-to-root ".")

  (let ((arduino-buffer "*arduino-logs*"))
    (get-buffer arduino-buffer)
    (switch-to-buffer-other-window arduino-buffer)

    (when (not (equal (get-buffer board-port) nil))
      (kill-buffer board-port))

    (call-process "arduino-cli" nil arduino-buffer t "compile"
  		"--fqbn" board-name
  		path-to-root)
    (call-process "arduino-cli" nil arduino-buffer t "upload"
  		"--port" board-port
  		"--fqbn" board-name
  		path-to-root)

    (serial-term board-port 9600)
    (switch-to-buffer board-port)))

(defun cam/does-arduino-program-work (&optional board-name path-to-root)
  "Tries to compile the program, and will
whine if it doesn't.

It'll tell you if it works or not in the
minibuffer."
  (interactive)
  (cam/default-value board-name "arduino:avr:uno")
  (cam/default-value path-to-root ".")

  (if (equal 1 (call-process "arduino-cli" nil "*arduino-logs*" t
                           "compile"
                           "--fqbn" board-name
                           path-to-root))

      (switch-to-buffer-other-window "*arduino-logs*")

    (message "Yeah, it works :) .")))

(defun cam/test-arduino-from-org (&optional tangle-file)
  "Bla bla bla faggot shit"
  (interactive)
  (cam/default-value tangle-file (current-buffer))

  (org-babel-tangle nil (current-buffer)))

(use-package arduino-mode
  :ensure t
  :bind (("C-c M-c" . cam/arduino-compile-and-load)
       ("C-c M-t" . cam/does-arduino-program-work))
  :config
  (add-to-list 'auto-mode-alist (cam/file-extension-regex "ino" 'arduino-mode)))

(add-to-list 'auto-mode-alist (cam/file-extension-regex "hc" 'c-mode))

(use-package magit
  :straight t)

(setq org-directory (concat (getenv "HOME") "/Documents/Org")
      org-agenda-files (directory-files-recursively org-directory
  						  (rx bol
  						      (one-or-more (or lower-case ?-))
  						      ".org" eol)))

(global-set-key (kbd "C-c a") 'org-agenda)

(use-package casual-agenda
  :straight nil
  :ensure t
  :bind (:map org-agenda-mode-map ("C-o" . casual-agenda-tmenu))
  :after (org-agenda))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-agenda-directory (concat org-directory "/Agenda")
      org-agenda-work-file (concat org-agenda-directory "/work.org")
      org-agenda-social-file (concat org-agenda-directory "/social.org")
      org-agenda-personal-file (concat org-agenda-directory "/personal.org"))

(setq org-capture-templates
      '(("u" "Uni Stuff" entry (file+headline org-agenda-work-file "Uni")
         "* TODO [#C] %?\nSCHEDULED: %t")
      ("s" "Social Stuff" entry (file+headline org-agenda-social-file "Misc")
       "* TODO [#C] %?\nSCHEDULED: %t")
      ("p" "Personal" entry (file+headline org-agenda-personal-file "Stuff ToDo")
       "* TODO [#C] %?\nSCHEDULED: %t")))

(global-set-key (kbd "C-c c") 'org-capture)

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)


(prot-window-define-with-popup-frame org-capture)
(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

(add-to-list 'org-structure-template-alist '("ll" . "src elisp"))
(add-to-list 'org-structure-template-alist '("ls" . "src scheme"))
(add-to-list 'org-structure-template-alist '("lh" . "src haskell"))
(add-to-list 'org-structure-template-alist '("la" . "src cpp")) ;; for arduino

(setq org-html-validation-link nil
      org-export-with-author nil
      org-export-with-toc nil)

;; (use-package ox-latex
;;   :config
;;   (setq org-latex-listings 'minted)
;;   (add-to-list 'org-export-latex-package-alist '("" "minted")))


;;(add-to-list org-export-latex-package-alist '("" "minted"))
(setq org-latex-listings 'minted
      org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(require 'org-bullets)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)
(setq org-startup-folded t)


(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "deep sky blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(org-level-8 ((t (,@headline ,@variable-tuple))))
 '(org-level-7 ((t (,@headline ,@variable-tuple))))
 '(org-level-6 ((t (,@headline ,@variable-tuple))))
 '(org-level-5 ((t (,@headline ,@variable-tuple))))
 '(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
 '(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
 '(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
 '(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
 '(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))

(setq erc-server "irc.libera.chat"
      etc-track-shorten-start 8
      erc-kill-buffer-on-part t
      erc-auto-query 'bury
      erc-user-full-name "cam a."
      erc-nick "vibe876")

(global-set-key (kbd "M-p e") 'erc-tls)

(use-package mu4e
  :ensure nil
  :straight nil
  :defer t
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands (mu4e)
  :config
  (setq mu4e-maildir (expand-file-name "~/Documents/email")
        mu4e-drafts-folder "/drafts"
        mu4e-sent-folder "/sent-items"
        mu4e-trash-folder  "/trash"
        mu4e-show-images t
        mail-user-agent 'mu4e-user-agent))

(use-package emms
  :straight t
  :defer t
  :config
  (setq emms-player-list '(emms-player-mpv)
        emms-source-file-default-directory "/home/cam/Music/music"))

(emms-all)

(transient-define-prefix cam/emms-transient nil
  "Just an emms transient menu."
  [["start/stop"
    ("s" "start" emms-start)
    ("p" "pause" emms-pause)
    ("S" "stop"  emms-stop)]
   ["misc"
    ("n" "next"  emms-next)]])

(global-set-key (kbd "C-c r e") 'cam/emms-transient)

(use-package elpher
  :straight t)

(use-package eradio
  :straight t
  :bind (("C-c r p" . eradio-play)
         ("C-c r s" . eradio-stop)
         ("C-c r t" . eradio-toggle))
  :config (setq eradio-player '("mpv" "--no-video" "--no-terminal")
                eradio-channels '(("lush - soma fm"         . "https://somafm.com/lush.pls")
                                  ("defcon - soma fm"       . "https://somafm.com/defcon256.pls")
                                  ("deep space - soma fm"   . "https://somafm.com/deepspaceone.pls")
                                  ("ind p-r - soma fm"      . "https://somafm.com/indiepop.pls")
                                  ("trippin - soma fm"      . "https://somafm.com/thetrip.pls")
                                  ("dark ambient - soma fm" . "https://somafm.com/darkzone256.pls")
                                  ("celtic - some fm"       . "https://somafm.com/thistle.pls")
                                  ("analog rock - soma fm"  . "https://somafm.com/digitalis256.pls")
                                  ("n5MD - soma fm"         . "https://somafm.com/n5md.pls")
                                  ("drone zone - soma fm"   . "https://somafm.com/dronezone256.pls")
                                  ("vaporwave - soma fm"    . "https://somafm.com/vaporwaves.pls")
                                  ("mellow rock - some fm"  . "https://somafm.com/seventies320.pls")
                                  ("dark ind amb - soma fm" . "https://somafm.com/doomed256.pls"))))

(use-package elfeed
  :straight t
  :bind ("C-c e" . elfeed)
  :config
  ;; (add-hook 'elfeed-new-entry-hook
  ;;         (elfeed-make-tagger :before "2 months ago"
  ;;                             :remove 'unread))
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
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCIPfjC8FVLdul4-35JekB1g" video spooky) ;; Real Horror
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtMVHI3AJD4Qk4hcbZnI9ZQ" video blog) ;; SOG
          )))

;; Should add this to the use-package...
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :before "2 months ago"
;;                               :remove 'unread))

(use-package ace-window
  :straight t
  :demand t
  :config
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-always t
      aw-background nil))

(use-package avy
  :straight t
  :demand t
  :config
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1))

(use-package casual-avy
  :bind ("M-g g" . casual-avy-tmenu))

(use-package ef-themes
  :defer nil
  :demand t
  :config
  (load-theme 'ef-dream t))

(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

(use-package nyan-mode
  :straight t)

(nyan-mode)

(use-package casual-dired
  :straight nil
  :defer t
  :bind (:map dired-mode-map
	    ("C-o" . casual-dired-tmenu)))

(use-package poker
  :straight t)

(use-package 2048-game
  :straight t)

(use-package server
  :ensure nil
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(global-display-line-numbers-mode)

(set-face-attribute 'default nil :height 110)

(setq scroll-step 1)

(setq dired-vc-rename-file t
      dired-kill-when-opening-new-dired-buffer t)


(add-hook 'dired-mode-hook (lambda nil
                           (dired-hide-details-mode 1)))

(add-hook 'dired-mode-hook (lambda nil
                           (dired-omit-mode 1)))


(use-package which-key
  :straight t)

(use-package casual-info
  :straight nil
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

(use-package which-key
  :straight t
  :defer nil
  :config
  (which-key-mode))

(defun display-startup-echo-area-message nil
  "I don't want gnu's stuff at startup, I want my stuff."
  (message "Hello Cam :) ."))

(setq ispell-program-name "hunspell")
(global-set-key (kbd "M-£") 'ispell-region)

(use-package focus
  :straight t)

(setq gc-cons-threshold (* 2 1000 1000))
