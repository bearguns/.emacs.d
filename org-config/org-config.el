;;; org-config.el --- org-mode configuration by Sean
;;; Commentary:
;; Author Information:
;; Sean Brage 2018

;;; Code:
;;; list and install org-mode packages
(require 'package)

(setq org-packages
      '(org-pomodoro
        org-bullets
        org-journal
        ))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p org-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; configure installed packages
(require 'org-pomodoro)
(require 'org-bullets)
(require 'org-journal)

(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode 1)
                           (org-bullets-mode 1)))

;; org-mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; org journal stuff
(setq-default org-journal-dir "~/Dropbox/org")
(setq-default org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(provide 'org-config)
;;; org-config ends here
