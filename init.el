;; init.el --- emacs configuration by Sean
;; Commentary:
;; Custom configuration, some copied from Andrew Jarrett's excellent config
;; Author Information
;;; Code:
(setq inhibit-startup-message t)

;; configure package management and installation
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; use use-package because it's what people use
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

;; set PATH and EXEC-PATH stuff
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; read configuration from org file
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "7559ac0083d1f08a46f65920303f970898a3d80f05905d01e81d49bb4c7f9e39" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "d70c11f5a2b69a77f9d56eff42090138721d4c51d9d39ce986680786d694f492" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" default)))
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (solarized-theme color-theme-sanityinc-tomorrow atom-one-dark-theme airline-themes powerline base16-theme treemacs-projectile treemacs leuven-theme web-mode vue-mode use-package ubuntu-theme tide solaire-mode smartparens smart-mode-line rjsx-mode rainbow-delimiters pipenv org-pomodoro org-journal org-bullets nyan-mode moe-theme material-theme magit exec-path-from-shell emmet-mode elscreen elpy dracula-theme doom-themes counsel-projectile better-defaults ample-theme ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
