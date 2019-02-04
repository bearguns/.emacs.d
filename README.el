(use-package better-defaults
  :ensure t
  :init 
  (require 'better-defaults)
  (setq ring-bell-function 'ignore))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package treemacs
  :ensure t
)

;; Change 'yes or no' options to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

;; Start the server (for opening files from external sources in the current Emacs instance)
(server-start)

;; use ibuffer by default
(defalias 'list-buffers 'ibuffer)

;; Set a decent window size:
(add-to-list 'default-frame-alist '(height . 64))
(add-to-list 'default-frame-alist '(width . 180))

;; Remove os GUI stuff, it's ugly
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Source Code Pro font installed via Homebrew on macOS
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 140)

(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil))

(use-package powerline
:ensure t
:config (powerline-default-theme))

(use-package nyan-mode
  :ensure t
  :defer t
  :init
  (add-hook 'nyan-mode-hook 'nyan-start-animation)
  (add-hook 'after-init-hook 'nyan-mode 1)
  (setq-default nyan-animate-nyancat t)
  (setq-default nyan-wavy-trail t))

(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :defer t
  :init
  (require 'smartparens-config)
  ;; use smartparens everywhere
  (smartparens-global-mode 1))

(use-package ace-window
  :ensure t
  :defer t
  :init (global-set-key (kbd "M-o") 'ace-window))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package magit
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-x g") 'magit-status))

;; counsel (ivy) for better ido-like completion in emacs
(use-package counsel
  :ensure t
  :init
  ;; turn it on plz
  (ivy-mode 1)
  ;; Default settings
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char))

;; counsel-projectile provides wonderful project-based shortcuts and completion
(use-package counsel-projectile
  :ensure t)

(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package company
  :ensure t
  :defer t
  :init 
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (global-company-mode 1))

(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode 1))

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'vue-mode-hook #'emmet-mode)
  (add-hook 'js2-mode-hook #'emmet-mode)
  (add-hook 'rjsx-mode-hook #'emmet-mode))

(use-package web-mode
  :ensure t
  :init
  (setq-default web-mode-enable-auto-pairing t)
  (setq-default web-mode-enable-auto-closing t)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2))

(setq-default css-indent-offset 2)

(use-package vue-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 0))

(use-package js2-mode
  :ensure t
  :init
  (setq-default js2-basic-offset 2)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js-indent-level 2))

;; store org files in Dropbox
(setq-default org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
;; fill columns in org mode (keep lines from going on into infinity)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
;; org-mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-pomodoro
  :ensure t
  :init (require 'org-pomodoro))

(use-package org-journal
  :ensure t
  :init (setq-default org-journal-dir "~/Dropbox/org"))
