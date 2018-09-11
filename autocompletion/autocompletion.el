;;; autocompletion.el --- autocompletion settings by Sean
;;; Commentary:
;; provide autocompletion for coding and editing lisp files

;;; Code:
;; list and install packages
(require 'package)
(defvar autocompletion-packages
  '(company
    counsel
    counsel-projectile))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (p autocompletion-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; configure installed packages
(with-eval-after-load "autocompletion"
  (ivy-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)
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
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (ac-config-default))

(provide 'autocompletion)
;;; autocompletion.el ends here
