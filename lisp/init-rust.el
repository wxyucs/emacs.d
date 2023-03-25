;;; init-rust.el --- Defaults for rust -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download rust-mode
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))
(unless (package-installed-p 'rustic)
  (package-install 'rustic))

(require 'rust-mode)
(require 'rustic)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))

(setq rust-format-on-save t)

(add-hook 'rust-mode-hook 'lsp-deferred)

(provide 'init-rust)
;;; init-rust.el ends here
