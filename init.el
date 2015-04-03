;;; init.el --- James Wang's Emacs init file
;;
;; Author: James Wang
;; Version 1
;;

;;; Commentary:
;; For loading up code not tied to specific packages and calling
;; package code in my-packages.el and my-loadpackages.el

;;; Code:
;; Load Packages
(load "~/.emacs.d/my-packages.el")
(load "~/.emacs.d/my-loadpackages.el")

;; Don't load GNU Emacs Screen
(setq inhibit-startup-screen t)

;; Load Custom Files

;; Set mode-line
(size-indication-mode)
(column-number-mode)

;; Load theme
(load-theme 'solarized-dark t)

;; Flycheck everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Activate CEDET
(semantic-mode 1)
(global-ede-mode 1)
(global-semantic-idle-summary-mode 1)

;;; init.el ends here
