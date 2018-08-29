;;; org-config.el --- org-mode configuration by Sean
;;; Commentary:
;; Author Information:
;; Sean Brage 2018

;;; Code:
;;; list and install org-mode packages
(require 'package)
(package-initialize)
(defvar init-packages
  '(org-pomodoro
    ))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p init-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
(require 'org-pomodoro)

;; org-mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq-default org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(provide 'org-config)
;;; org-config ends here
