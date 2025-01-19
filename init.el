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

(straight-use-package 'ef-themes)
(load-theme 'ef-cherie t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defalias 'yes-or-no-p 'y-o-n-p)
