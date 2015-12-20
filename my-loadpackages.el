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

(require 'latex-preview-pane)
(latex-preview-pane-enable)

(require 'centered-window-mode)

(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-default-width 20)
(setq sr-speedbar-max-width 30)

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

(require 'flycheck)
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

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
