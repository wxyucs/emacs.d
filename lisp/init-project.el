;;; init-project.el --- Defaults for theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download projectile
(unless (package-installed-p 'projectile )
  (package-install 'projectile ))

;; Enable Evil
(require 'projectile)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(provide 'init-project)
;;; init-projectile.el ends here
