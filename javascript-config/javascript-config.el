;;; javascript.el - JS development configuration by Sean
;;; Commentary:
;; provide autocompletion, indentation, linting, etc.

;;; Code:
(require 'package)
(setq javascript-packages
  '(js2-mode
    web-mode
    vue-mode
    rjsx-mode
    emmet-mode
    graphql-mode))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p javascript-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; configure installed packages
(require 'js2-mode)
(require 'web-mode)
(require 'vue-mode)
(require 'emmet-mode)

;; use emmet!
(add-hook 'web-mode-hook #'emmet-mode)
(add-hook 'vue-mode-hook #'emmet-mode)
(add-hook 'js2-mode-hook #'emmet-mode)

(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode))

(defun my-rjsx-mode-hook ()
  (setq emmet-expand-jsx-className? t))
(add-hook 'rjsx-mode-hook 'my-rjsx-mode-hook)

(setq-default js2-basic-offset 2)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default graphql-indent-level 2)

;; vuejs
(setq-default vue-html-tab-width 2)
(setq-default mmm-submode-decoration-level 0)
(setq-default mmm-default-submode-face nil)

;; web-mode
; Try and improve web-mode experience
(defun my-web-mode-hook ()
  "Set reasonable indentation and settings in web-mode."
  (setq-default web-mode-enable-auto-pairing t)
  (setq-default web-mode-enable-auto-closing t)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(provide 'javascript)
;;; javascript.el ends here
