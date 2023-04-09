;;; init-treemacs.el --- Defaults for treemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download treemacs
(setq package-selected-packages '(treemacs treemacs-evil treemacs-projectile))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

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
(global-set-key (kbd "M-o") 'ace-window)

;; a quick fix on https://github.com/Alexander-Miller/treemacs/issues/1009
(require 'ace-window)
(defun -ignore-arguments (orig &rest args) (funcall orig))
(add-function :around (symbol-function 'ace-select-window) #'-ignore-arguments)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
