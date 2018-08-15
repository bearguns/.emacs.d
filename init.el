;;; init.el --- emacs configuration by Sean
;;; Commentary:
;; Author Information

;;; Code:
(package-initialize)

(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages) ;;custom package to load packages with your packages
(require 'development) ;;custom dev-specific stuff
(require 'better-defaults)
(require 'neotree)

(require 'editorconfig)

(editorconfig-mode 1)

(global-set-key [f8] 'neotree-toggle)
(load-theme 'nord t)

;;; HELM
(add-to-list 'load-path "~/.emacs.d/elpa/helm")
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; WARNING THIS IS DUMB
(nyan-mode 1)
(setq nyan-animate-nyancat t)
(setq nyan-wavy-trail t)
(nyan-start-animation)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally


;; Set default font
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 160
                    :weight 'normal
                    :width 'normal)
;; Super Basic Editor Settings
(fset 'yes-or-no-p 'y-or-n-p)

;;;No Backup Files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally
;;;; Text/Visual Stuff

(show-paren-mode t) ;;highlight matching parens

;; customize company-mode
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-minimum-prefix-length 1)

;;Deal With Trailing Whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("9240e71034689655a6c05c04063af2c90d0a831aa4e7ca24c8b6e29b5a2da946" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" default)))
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (lua-mode counsel-projectile company-jedi rjsx-mode rainbow-delimiters py-autopep8 pipenv markdown-mode magit json-mode flycheck elpy cyberpunk-theme better-defaults autopair)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
