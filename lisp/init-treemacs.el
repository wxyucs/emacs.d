;;; init-treemacs.el --- Defaults for treemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download treemacs
(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))

;; Enable treemacs
(require 'treemacs)
(require 'treemacs-evil)

(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t g") 'treemacs-select-window)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
