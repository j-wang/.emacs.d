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

;; Set default tab-width
(setq-default tab-width 4)

;; C settings
(setq c-default-style "linux"
      c-basic-offset 4)

;; Make Emacs prettier
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)
(setq-default frame-title-format '("%b - %F"))

(set-face-attribute 'default nil :font "Consolas")

;; Add margins
(defun my-set-text-properties ()
  "Set margins in current buffer, and prettiness."
  (Visual-line-mode 1)
  (line-height 1.2))

(add-hook 'text-mode-hook 'my-set-text-properties)
(add-hook 'markdown-mode-hook 'my-set-text-properties)

;; Load Custom Files

;; Set mode-line
(size-indication-mode)
(column-number-mode)

;; Load theme
(load-theme 'zenburn t)

;; Flycheck everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Activate CEDET
(semantic-mode 1)
(global-ede-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-completions-mode 1)
(setq ede-arduino-appdir "/Applications/Arduino.app/Contents/Java")


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(ede-project-directories (quote ("/Users/James/dev/sb_firmware"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
