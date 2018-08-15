;;; install-packages.el --- Emacs Package Management
;;; Commentary:
;; installs packages, and that's it!

;;; Code:
(require 'package)

(defvar my-packages
  '(better-defaults
    company
    nyan-mode
    editorconfig
    nord-theme
    elpy
    autopair
    async ;; required for helm
    popup ;; required for helm
    projectile
    helm-projectile
    counsel
    swiper
    json-mode
    magit
    markdown-mode
    py-autopep8
    pipenv
    rainbow-delimiters
    js2-mode
    rjsx-mode
    exec-path-from-shell
    flycheck
    web-mode
    ace-window
    org-pomodoro
    neotree))

(add-to-list
 'package-archives
 ;;'("melpa" . "http://melpa.org/packages/")
 '("melpa" ."http://melpa.milkbox.net/packages/")t)

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)
;;; install-packages.el ends here
