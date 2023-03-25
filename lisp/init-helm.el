;;; init-heml.el --- Defaults for helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download helm-mode
(unless (package-installed-p 'helm)
  (package-install 'helm))

(require 'helm)

(helm-mode)

(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(global-set-key (kbd "C-x c M-g a") #'helm-do-grep-ag)

(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

(provide 'init-helm)
;;; init-helm.el ends here