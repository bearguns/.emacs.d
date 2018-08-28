;;; python.el --- emacs python settings by Sean
;;; Commentary:
;; provide things like pipenv integration and python auto-completion

;;; Code:

;;; list and install python packages
(require 'package)
(defvar python-packages
  '(elpy
    pipenv))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p python-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
(require 'elpy)
(elpy-enable)
(add-hook 'python-mode-hook #'elpy-enable)

(require 'pipenv)
(add-hook 'python-mode-hook #'pipenv-mode)
(add-hook 'python-mode-hook #'pipenv-activate)

(provide 'python)
;;; python.el ends here
