;;; init.el --- emacs configuration by Sean
;;; Commentary:
;; Author Information

;;; Code:
(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

;;; list and install core packages
(require 'package)
(defvar init-packages
  '(better-defaults
    editorconfig
    elscreen
    exec-path-from-shell
    smartparens
    dashboard
    magit
    ace-window
    neotree
    counsel
    projectile
    counsel-projectile))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p init-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
(package-initialize)
(require 'ace-window)
(require 'better-defaults)
(require 'neotree)
(require 'elscreen)
(require 'smartparens)
(require 'smartparens-config)
(require 'magit)

(add-hook 'after-init-hook #'editorconfig-mode 1)
(add-hook 'after-init-hook #'projectile-mode 1)

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



;; Global Keybindings
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c g") 'magit)

;; Change 'yes or no' options to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq inhibit-startup-message t)

(global-linum-mode t)

;; Set font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 150
                    :weight 'normal
                    :width 'normal)


;;; load custom configuration packages from ./
(require 'theme)
(require 'org)
(require 'python)
(require 'javascript)
(require 'autocompletion)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" default)))
 '(elpy-mode-hook (quote (subword-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

