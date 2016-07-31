;;; my-loadpackages.el
(load "~/.emacs.d/my-packages.el")
(load "~/.emacs.d/go-autocomplete.el")

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

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'latex-preview-pane)
(latex-preview-pane-enable)

(require 'project-explorer)

(require 'eval-in-repl)
;;; Python support
;; (require 'python) ; if not done elsewhere
(require 'eval-in-repl-python)
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))

;; Activate Helm
(require 'helm-config)

(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)

(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) 
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode 1)
(helm-autoresize-mode 1)

;; Dumb Jump
(dumb-jump-mode)

;; Python loads
(require 'elpy)
(elpy-enable)
(add-hook 'python-mode-hook
	  (lambda()
	    (auto-fill-mode 1)
	    (linum-mode 1)))

;; Web-Mode for JSX
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?$")))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))
(setq-default indent-tabs-mode nil)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(require 'flycheck)
;; Add ESlint, remove jshint
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(json-jsonlist)))

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

;; Go Stuff
(require 'go-mode)
(setenv "GOPATH" (concat (getenv "HOME") "/Documents/Dev/Go"))

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;; gofmt on save
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path (concat (getenv "HOME") "/Documents/Dev/Go/bin"))
(add-hook 'before-save-hook 'gofmt-before-save)

;; definition jump with M-.
(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
