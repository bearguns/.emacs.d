;; init.el --- emacs configuration by Sean
;;; Commentary:
;; Custom configuration, some copied from Andrew Jarrett's excellent config
;; Author Information
;;; Code:
;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

; use use-package because it's what people use
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq-default use-package-always-ensure t)
(require 'use-package)

;; Appearance
(use-package overcast-theme
  :init (load-theme 'overcast t))
;; use a nice, low-noise theme
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; remove OS/DE chrome
(set-face-attribute 'default nil :height 140)
;; default font at a decent height on most monitors

;; Default Behavior
(fset 'yes-or-no-p 'y-or-n-p)
;; allow y/n to confirm options
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
;; no backups or locks
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;; show line numbers in newer versions of emacs
;; Working with Text
(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode 1))
;; yasnippets is an excellent snippet manager
(electric-pair-mode 1)
;; electric pair mode provides what you'd want from smartparens/autopairs out-of-the-box

;; Navigating & Managing Files
(defalias 'list-buffers 'ibuffer)
;;; use ibuffer by default when pressing C-x b
(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c i") 'ivy-resume)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))
;;; counsel provides excellent ido-like completion tools

;; Programming
(show-paren-mode 1)
;; show matching parens, very helpful
(use-package rainbow-delimiters
  :init
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode))
;; colorize nested delimiters for easy reading
(use-package company
  :init
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'js2-mode-hook 'company-mode))
;; company for auto-completion in buffers
(use-package magit
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-x g") 'magit-status))
;; Magit for working in Git repositories
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
;; Editorconfig for applying consistent settings in projects with multiple users.
(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  :init
  (global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines))
;; Flycheck for identifying errors in buffers
(use-package emmet-mode)
;; emmet for expanding html elements
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'electric-pair-mode)
  (defun web-mode-indent-vue ()
      (setq-local web-mode-style-padding 0)
      (setq-local web-mode-script-padding 0))
  (add-hook 'web-mode-hook 'web-mode-indent-vue)
  (defvar web-mode-electric-pairs '((?\' . ?\')))
  (defun web-mode-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs web-mode-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (add-hook 'web-mode-hook 'web-mode-add-electric-pairs))
;; web-mode for HTML + VueJS
(use-package js2-mode
  :init
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
;; nice mode for working in javascript files
(use-package prettier-js
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode))
;; prettier-js code formatting
;; org-mode
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(setq-default org-startup-indented t)
;; org-mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
;; org-mode defaults I prefer
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))
;;; init.el ends here
