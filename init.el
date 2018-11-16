;; init.el --- emacs configuration by Sean
;; Commentary:
;; Custom configuration, some copied from Andrew Jarrett's excellent config
;; Author Information
;;; Code:
(setq inhibit-startup-message t)

;; configure package management and installation
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; use use-package because it's what people use
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

;; set PATH and EXEC-PATH stuff
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; read configuration from org file
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :foreground "white" :underline "white")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-journal org-pomodoro org-bullets tide rjsx-mode js2-mode web-mode emmet-mode yasnippet company counsel-projectile counsel magit elscreen ace-window smartparens rainbow-delimiters nyan-mode solaire-mode doom-themes use-package))))
