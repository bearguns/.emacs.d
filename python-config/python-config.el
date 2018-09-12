;;; python.el --- emacs python settings by Sean
;;; Commentary:
;; provide things like pipenv integration and python auto-completion

;;; Code:
;;; list and install python packages
(require 'package)
(setq python-packages
  '(elpy
    pipenv))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p python-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; configure installed packages
(with-eval-after-load "python-config"
    (defun my-python-mode-hook ()
      (elpy-mode)
      (pipenv-mode)
    (add-hook 'python-mode-hook 'my-python-mode-hook)))

(provide 'python)
;;; python.el ends here
