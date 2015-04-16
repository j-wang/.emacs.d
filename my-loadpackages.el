;;; my-loadpackages.el
(load "~/.emacs.d/my-packages.el")

;; Whitespace-mode (built-in)
(require 'whitespace)

(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")  ;; don't show instructions

(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))


(require 'arduino-mode)

;; Activate Helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)

;; Python loads
(require 'elpy)
(elpy-enable)
(add-hook 'python-mode-hook
	  (lambda()
	    (auto-fill-mode 1)
	    (linum-mode 1)))

;; Javascript stuff
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;; Future -- add creation of .tern-project automatically


;; Enable paredit for lispy files
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Ensure that emacs sees the same $PATH as shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

