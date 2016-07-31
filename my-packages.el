;;; my-packages.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Get required packages
(defvar required-packages
  '(
    magit
    yasnippet
    multiple-cursors
    solarized-theme
    zenburn-theme
    elpy
    paredit
    exec-path-from-shell
    evil
    evil-surround
    flycheck
    markdown-mode
    async
    helm
	helm-ag
	helm-projectile
	dumb-jump
    js2-mode
    json-mode
    coffee-mode
    scss-mode
    ac-js2
    web-mode
	projectile
    project-explorer
    latex-preview-pane
    go-mode
    elm-mode
    eval-in-repl
    ) "list of packages to ensure are installed at launch")

;; Check if packages are installed
(defun packages-installed-p ()
  (let ((my-p-installed t))
    (dolist (p required-packages my-p-installed)
      (setq my-p-installed
	    (and my-p-installed (package-installed-p p))))))

;; if not all packages installed, install missing
(unless (packages-installed-p)
  (message "%s" "Now refreshing package database...")
  (package-refresh-contents)
  (message "%s" "Done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
