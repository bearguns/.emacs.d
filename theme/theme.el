

;;; theme.el --- Emacs UI & Theme Customization
;;; Commentary:
;; loads doom-themes, selects a color theme, and customizes things like the modeline and neotree

;;; Code:
(require 'package)
;; list and install packages
(defvar layer-packages
  '(
    doom-themes
    nyan-mode
    neotree
    rainbow-delimiters))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p layer-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages

;;; nyan mode
(require 'nyan-mode)
(add-hook 'nyan-mode-hook 'nyan-start-animation)
(add-hook 'after-init-hook 'nyan-mode 1)
(setq-default nyan-animate-nyancat t)
(setq-default nyan-wavy-trail t)

;;; color theme
(require 'doom-themes)
(load-theme 'doom-dracula)
(setq custom-safe-themes '(
                           doom-dracula))
(doom-themes-neotree-config)
(doom-themes-org-config)
(doom-themes-visual-bell-config)
;;; rainbow delimiters
(require 'rainbow-delimiters)
(rainbow-delimiters-mode)

(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))

(provide 'theme)
;;; theme.el ends here
