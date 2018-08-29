;;; autocompletion.el --- autocompletion settings by Sean
;;; Commentary:
;; provide autocompletion for coding and editing lisp files

;;; Code:
;; list and install packages
(require 'package)
(defvar autocompletion-packages
  '(auto-complete))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p autocompletion-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
(with-eval-after-load "autocompletion"
  (ac-config-default))

(provide 'autocompletion)
;;; autocompletion.el ends here
