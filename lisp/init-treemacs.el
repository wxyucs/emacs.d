;;; init-treemacs.el --- Defaults for treemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download treemacs
(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))
(unless (package-installed-p 'treemacs-evil)
  (package-install 'treemacs-evil))
(unless (package-installed-p 'treemacs-projectile)
  (package-install 'treemacs-projectile))

;; Enable treemacs
(require 'treemacs)
(require 'treemacs-evil)
(require 'treemacs-projectile)

(add-hook 'emacs-startup-hook 'treemacs)
(treemacs-tag-follow-mode)

(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t g") 'treemacs-select-window)
(global-set-key (kbd "C-x t a") 'treemacs-add-and-display-current-project)
(global-set-key (kbd "C-x t e") 'treemacs-display-current-project-exclusively)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
