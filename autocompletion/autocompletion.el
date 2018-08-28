;;; autocompletion.el --- company autocompletion settings by Sean
;;; Commentary:
;; provide autocompletion for coding and editing lisp files

;;; Code:
;; list and install packages
(require 'package)
(defvar company-packages
  '(company
    company-jedi))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p company-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
;; enable company-jedi
(require 'company)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'my/python-mode-hook)

(provide 'autocompletion)
;;; company.el ends here
