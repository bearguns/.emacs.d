;;; autocompletion.el --- autocompletion settings by Sean
;;; Commentary:
;; provide autocompletion for coding and editing lisp files

;;; Code:
;; list and install packages
(require 'package)
(setq autocompletion-packages
  '(auto-complete
    counsel
    counsel-projectile))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p autocompletion-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; configure installed packages
(with-eval-after-load "autocompletion"
  (dolist (p autocompletion-packages)
    (require p))
  (ivy-mode 1)
  (ac-config-default)
  (setq ac-use-menu-map t)
;; Default settings
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (setq ac-delay 0.2)
  (setq ac-auto-start 2)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox))

(provide 'autocompletion)
;;; autocompletion.el ends here
