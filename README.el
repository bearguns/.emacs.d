;; Remove os GUI stuff, it's ugly
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; load atom-one-dark theme
(use-package atom-one-dark-theme
:init (load-theme 'atom-one-dark t))

(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 140)

;; Change 'yes or no' options to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

;; use ibuffer by default
(defalias 'list-buffers 'ibuffer)

(use-package magit
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-x g") 'magit-status))

(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package smartparens
  :init 
  (require 'smartparens-config)
  (add-hook 'lisp-mode-hook #'smartparens-mode)
  (add-hook 'web-mode-hook #'smartparents-strict-mode)
  (add-hook 'js-mode-hook #'smartparens-strict-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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

(use-package company
  :ensure t
  :defer t
  :init 
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (add-hook 'lisp-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'js-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'company-mode))

(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode 1))

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'js2-mode-hook #'emmet-mode)
  (add-hook 'rjsx-mode-hook #'emmet-mode))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (setq-default web-mode-enable-auto-closing t)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2))

(setq-default css-indent-offset 2)

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
