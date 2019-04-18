(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
;; install the main package
(use-package page-break-lines)
;; install a dependency the dashboard needs to make pretty lines

;; Remove os GUI stuff, it's ugly
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :height 140)

(defun set-light-theme ()
  "Set the light theme with some customization if needed."
  (interactive)
  (load-theme 'adwaita)) ;;adwaita is a nice light theme with a slightly off-white background

(defun set-dark-theme ()
  "Set the dark theme with some customization if needed."
  (interactive)
  (load-theme 'deeper-blue))

(setq current-hour (string-to-number (format-time-string "%H")))
(if (< current-hour 17) 
  (set-light-theme)
  (set-dark-theme))

(use-package nyan-mode
  :init
  (nyan-mode 1)
  (setq-default nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (nyan-start-animation))

(use-package ace-window
  :init (global-set-key (kbd "M-o") 'ace-window))

;; Change 'yes or no' options to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

;; use ibuffer by default
(defalias 'list-buffers 'ibuffer)

(server-start)

(use-package magit
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-x g") 'magit-status))

(show-paren-mode 1)

(use-package rainbow-delimiters
  :init
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; counsel (ivy) for better ido-like completion in emacs
(use-package counsel
  :ensure t
  :init
  ;; turn it on plz
  (ivy-mode 1)
  ;; Default settings
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package company
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'js-mode-hook #'company-mode)
  (add-hook 'js2-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'company-mode)
  :init 
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil))

(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode 1))

(use-package prettier-js
    :config 
    (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'js-mode-hook #'emmet-mode))

(use-package js2-mode
  :init
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-script-padding 0)
  ;; don't add indents for script tags, plays nicely with Vue.js
  (setq web-mode-style-padding 0)
  ;; don't add indents for style tags, plays nicely with Vue.js
  (setq-default web-mode-enable-auto-closing t))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.vue?\\'" . prettier-js-mode))))

(setq-default css-indent-offset 2)

;; store org files in Dropbox
(setq-default org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
;; fill columns in org mode (keep lines from going on into infinity)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
;; org-indent-mode makes it easier (imo) to visually read and scan in Org
(setq org-startup-indented t)
;; org-mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
