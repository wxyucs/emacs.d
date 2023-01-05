;;; init-theme.el --- Defaults for theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(provide 'init-evil)
;;; init-evil.el ends here
