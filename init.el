;;; init.el --- emacs configuration by Sean
;;; Commentary:
;; Author Information

;;; Code:
(package-initialize)
(exec-path-from-shell-initialize)
(dashboard-setup-startup-hook)

(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages) ;;custom package to load packages with your packages
(require 'development) ;;custom dev-specific stuff

(add-to-list 'load-path "~/.emacs.d/elpa/helm")
(add-hook 'nyan-mode-hook 'nyan-start-animation)
(add-hook 'after-init-hook 'nyan-mode 1)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq nyan-animate-nyancat t)
(setq nyan-wavy-trail t)

(helm-mode 1)
(show-paren-mode t)
(global-linum-mode t)

;; Set font
(set-face-attribute 'default nil
                    :family "Hack Nerd Font"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(load-theme 'doom-one)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "4a38c5fbfab06e761d43030d31b0970932188c5946b184211f0396b4125fbbc1" "57f95012730e3a03ebddb7f2925861ade87f53d5bbb255398357731a7b1ac0e0" "9240e71034689655a6c05c04063af2c90d0a831aa4e7ca24c8b6e29b5a2da946" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" default)))
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (doom-themes emmet-mode vue-mode lua-mode counsel-projectile company-jedi rjsx-mode rainbow-delimiters pipenv markdown-mode magit json-mode flycheck elpy cyberpunk-theme better-defaults)))
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
