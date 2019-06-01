;; init.el --- emacs configuration by Sean
;;; Commentary:
;; Custom configuration, some copied from Andrew Jarrett's excellent config
;; Author Information
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
;; initialize the built-in package library

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq-default use-package-always-ensure t)
(require 'use-package)
;; use use-package because it's what people use and it's nice

(use-package atom-one-dark-theme
  :init (load-theme 'atom-one-dark t))
;; use a nice, low-noise theme

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; remove OS/DE chrome

(set-face-attribute 'default nil
		    :font "DejaVuSansMono Nerd Font-15")
;; easy to read font at a decent height on most monitors

(use-package treemacs
  :ensure t
  :init (global-set-key (kbd "s-t") 'treemacs))

;; battery status
(use-package fancy-battery
  :init (fancy-battery-mode))

;; I couldn't resist
(use-package nyan-mode
  :config
  (setq-default nyan-animate-nyancat t)
  (setq-default nyan-wavy-trail t)
  (nyan-start-animation)
  :init
  (nyan-mode))
;; couple of handy modeline items

(fset 'yes-or-no-p 'y-or-n-p)
;; allow y/n to confirm options

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
;; no backups or locks

(when (version<= "26.0.50" emacs-version )
  (line-number-mode -1)
  (global-display-line-numbers-mode))
;; show line numbers in newer versions of emacs

(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode 1))
;; yasnippets is an excellent snippet manager

(electric-pair-mode 1)
;; electric pair mode provides what you'd want from smartparens/autopairs out-of-the-box

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
  :init
  (global-set-key (kbd "C-x g") 'magit-status))
;; Magit for working in Git repositories

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
;; Editorconfig for applying consistent settings in projects with multiple users.

(use-package add-node-modules-path
  :config
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'js2-mode-hook #'add-node-modules-path))


(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (global-flycheck-mode)
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side          . bottom)
		 (window-height . 0.25))))
;; Flycheck for identifying errors in buffers

(use-package emmet-mode)
;; emmet for expanding html elements

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'electric-pair-mode)

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
  :config
  (defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
  :init
  (add-hook 'web-mode-hook #'(lambda ()
			       (enable-minor-mode
				'("\\.vue?\\'" . prettier-js-mode))))
  (add-hook 'js2-mode-hook 'prettier-js-mode))
;; prettier-js code formatting

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(js2-mode . ("javascript-typescript-stdio"))))
;; language-server-protocol client

(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0)
  (setq vue-html-tab-width 0)
  :init
  (defclass eglot-vls (eglot-lsp-server) ()
    :documentation "Vue Language Server.")

  (add-to-list 'eglot-server-programs
	       '(vue-mode . (eglot-vls . ("vls" "--stdio")))
	       )

  (cl-defmethod eglot-initialization-options ((server eglot-vls))
    "Passes through required vetur initialization options to VLS."
    '(:vetur
      (:completion
       (:autoImport t :useScaffoldSnippets t :tagCasing "kebab")
       :grammar
       (:customBlocks
	(:docs "md" :i18n "json"))
       :validation
       (:template t :style t :script t)
       :format
       (:options
	:defaultFormatter
	(:css "prettier" :postcss "prettier" :scss "prettier" :js "prettier" :ts "prettier"))
       :trace
       (:server "verbose")
       :dev
       (:vlsPath ""))
      ))
  (add-hook 'vue-mode-hook 'electric-pair-mode))
;;vuejs mode

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
  :config
  (exec-path-from-shell-initialize))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(vue-html-tab-width 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
