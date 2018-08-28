;;; javascript.el - JS development configuration by Sean
;;; Commentary:
;; provide autocompletion, indentation, linting, etc.

;;; Code:
(require 'package)
(defvar javascript-packages
  '(js2-mode
    web-mode
    vue-mode
    emmet-mode))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p javascript-packages)
  (when (not (package-installed-p p))
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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(setq-default js2-basic-offset 2)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

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
