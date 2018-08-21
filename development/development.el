;;; development.el --- IDE configuration
;;; Commentary:
;;; This package provides package-specific configuration for my local dev environment.
;;; Code:
(global-set-key (kbd "M-o") 'ace-window)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))
(add-hook 'after-init-hook #'editorconfig-mode 1)
(add-hook 'after-init-hook #'projectile-mode 1)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode #'smartparens-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'smartparens-global-mode)
(add-hook 'web-mode-hook #'emmet-mode)
(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'python-mode-hook #'pipenv-mode)
(add-hook 'python-mode-hook #'pipenv-activate)
(global-set-key (kbd "C-c g") 'magit-status)

(setq-default mmm-submode-decoration-level 0)
(setq-default mmm-default-submode-face nil)
(setq-default web-mode-enable-current-element-highlight t)
(setq-default web-mode-enable-current-column-highlight t)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default vue-html-tab-width 2)
(setq-default company-idle-delay 0)
(setq-default company-echo-delay 0)
(setq-default company-minimum-prefix-length 1)

;;Deal With Trailing Whitespace
(setq-default show-trailing-whitespace t)
; Front-End App Syntax Checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint
                        json-jsonlint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq-default flycheck-temp-prefix ".flycheck")
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  "Tell flycheck to prefer project-specific instance of eslint over the global installation."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

; Try and improve web-mode experience
(defun my-web-mode-hook ()
  "Set reasonable indentation and settings in web-mode."
  (setq-default web-mode-enable-auto-pairing t)
  (setq-default web-mode-enable-auto-closing t)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))
(provide 'development)
;;; development.el ends here
