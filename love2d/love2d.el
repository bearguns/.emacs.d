;;; love2d.el --- basic config for working in lua and the love2d game engine
;;; Commentary:
;; provide lua auto-completion and highlighting, and easy compilation

;;; Code:
(require 'package)
(setq love2d-packages
      '(lua-mode))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (p love2d-packages)
  (unless (package-installed-p p)
    (package-install p)))

(provide 'love2d)
;;; love2d.el ends here
