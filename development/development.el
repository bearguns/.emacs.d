;;; development.el --- IDE configuration
;;; Commentary:
;;; This package provides package-specific configuration for my local dev environment.
;; Editor Stuff

;;; Code:
;; Ace Window (make it easier to move between splits)
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

(require 'projectile)
(projectile-mode 1)
;;; rainbow parens, brackets, etc
(require 'rainbow-delimiters)

;;; close brackets, quotes, parens
(require 'autopair)
(autopair-global-mode)

;; Magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

;; Fix My Code
(require 'flycheck)
(global-flycheck-mode)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Web
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . web-mode))

(setq js2-basic-offset 2)
(setq js-indent-level 2)

;; Python
(require 'elpy)
(elpy-enable)

(require 'pipenv)
;; Enable pipenv when working in Python
(add-hook 'python-mode-hook #'pipenv-mode)

(provide 'development)
;;; development.el ends here
