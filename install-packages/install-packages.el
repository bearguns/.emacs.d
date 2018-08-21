;;; install-packages.el --- Emacs Package Management
;;; Commentary:
;; installs packages, and that's it!

;;; Code:
(require 'package)

(defvar my-packages
  '(;; Text Editing
    better-defaults
    markdown-mode
    editorconfig
    smartparens
    rainbow-delimiters
    ;; UI
    doom-themes
    dashboard
    ace-window
    nyan-mode
    neotree
    async ;; required for helm
    popup ;; required for helm
    projectile
    helm-projectile
    ;; Programming
    company
    flycheck
    ;;; Python
    elpy
    py-autopep8
    pipenv
    magit
    ;;; Front-End
    json-mode
    js2-mode
    web-mode
    vue-mode
    emmet-mode
    ;; Utilities
    exec-path-from-shell
    org-pomodoro))

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 '("melpa" ."http://melpa.milkbox.net/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Require packages on startup so we have them available to configure in other init files
(require 'editorconfig)
(require 'emmet-mode)
(require 'dashboard)
(require 'neotree)
(require 'better-defaults)
(require 'ace-window)
(require 'projectile)
(require 'rainbow-delimiters)
(require 'magit)
(require 'flycheck)
(require 'company)
(require 'smartparens)
(require 'web-mode)
(require 'vue-mode)
(require 'elpy)
(require 'pipenv)
(require 'helm-config)
(require 'smartparens-config)
(provide 'install-packages)
;;; install-packages.el ends here
