

;;; init.el --- emacs configuration by Sean
;;; Commentary:
;; Author Information

;;; Code:
(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

;;; list and install core packages
(require 'package)

;; Add package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq init-packages
  '(better-defaults
    editorconfig
    elscreen
    exec-path-from-shell
    smartparens
    dashboard
    magit
    ace-window
    neotree
    projectile))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package init-packages)
  (unless (package-installed-p package)
    (package-install package)))

(load-library "theme")
(load-library "python-config")
(load-library "org-config")
(load-library "javascript-config")
(load-library "autocompletion")

;; configure installed packages
(require 'better-defaults)
(require 'neotree)
(require 'elscreen)
(require 'smartparens)
(require 'smartparens-config)
(require 'magit)

(editorconfig-mode 1)
(projectile-mode 1)

;; smartparens everywhere!
(smartparens-global-mode t)
(smartparens-strict-mode)
(elscreen-start)
(projectile-mode 1)
(ivy-mode 1)
(counsel-projectile-mode)
(dashboard-setup-startup-hook)
(exec-path-from-shell-initialize)

;; neotree options
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

;; dashboard
(setq dashboard-items '((recents . 5)
                        (bookmarks . 5)

                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

;; Global Keybindings
(global-set-key [f8] 'neotree-toggle)

;;; window stuff
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "C-c g") 'magit)

;; Change 'yes or no' options to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

(global-linum-mode t)

;; Set font
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ((doom-dracula)
     "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(package-selected-packages
   (quote
    (org-bullets counsel-projectile counsel graphql-mode emmet-mode rjsx-mode vue-mode web-mode js2-mode org-pomodoro pipenv elpy rainbow-delimiters solaire-mode nyan-mode doom-themes projectile neotree ace-window magit dashboard smartparens exec-path-from-shell elscreen editorconfig better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
