;;; python.el --- emacs python settings by Sean
;;; Commentary:
;; provide things like pipenv integration and python auto-completion

;;; Code:
;;; list and install python packages
(require 'package)
(defvar python-packages
  '(elpy
    pipenv
    jedi))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p python-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
(with-eval-after-load "python-config"
    (defun my-python-mode-hook ()
      (elpy-mode)
      (pipenv-mode)
      (setq jedi:setup-keys t)
      (setq jedi:complete-on-dot t))
    (add-hook 'python-mode-hook 'my-python-mode-hook)
    (add-hook 'python-mode-hook 'jedi:setup))

(provide 'python)
;;; python.el ends here
