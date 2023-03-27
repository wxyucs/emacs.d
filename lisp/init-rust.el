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
(add-hook 'rust-mode-hook 'lsp-deferred)

(setq rust-format-on-save t)

(add-to-list 'display-buffer-alist
                    `(,(rx bos "*cargo" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.3)))

(provide 'init-rust)
;;; init-rust.el ends here
