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

(load-library "theme")
(load-library "python-config")
(load-library "org-config")
(load-library "javascript-config")
(load-library "autocompletion")

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
    projectile))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p init-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
(require 'ace-window)
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
 '(ansi-color-names-vector
   ["#1E2029" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-safe-themes
   (quote
    ("f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" doom-dracula)))
 '(fci-rule-color "#6272a4")
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(package-selected-packages
   (quote
    (evil solaire-mode dracula-theme tide web-mode vue-mode smartparens rjsx-mode rainbow-delimiters py-autopep8 pipenv org-pomodoro nyan-mode nord-theme night-owl-theme neotree material-theme markdown-mode magit lua-mode json-mode jedi helm-projectile graphql-mode flycheck exec-path-from-shell emmet-mode elscreen elpy editorconfig doom-themes dashboard cyberpunk-theme counsel-projectile company-jedi better-defaults autopair atom-one-dark-theme ace-window)))
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
