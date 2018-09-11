

;;; theme.el --- Emacs UI & Theme Customization
;;; Commentary:
;; loads doom-themes, selects a color theme, and customizes things like the modeline and neotree

;;; Code:
(require 'package)
;; list and install packages
(defvar layer-packages
  '(doom-themes
    nyan-mode
    neotree
    solaire-mode
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
(add-to-list 'custom-safe-themes
             '(doom-dracula))
(doom-themes-neotree-config)
(doom-themes-org-config)
(doom-themes-visual-bell-config)
;;; rainbow delimiters
(require 'rainbow-delimiters)
(rainbow-delimiters-mode)

(add-to-list 'default-frame-alist '(height . 64))
(add-to-list 'default-frame-alist '(width . 180))

;;; praise the sun
(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'change-major-mode-hook #'turn-on-solaire-mode)
;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;; itself off every time Emacs reverts the file
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;;
;; NOTE: This is necessary for themes in the doom-themes package!
(solaire-mode-swap-bg)

(provide 'theme)
;;; theme.el ends here

