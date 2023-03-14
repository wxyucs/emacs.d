;;; init-rust.el --- Defaults for rust -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download rust-mode
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))
(unless (package-installed-p 'rustic)
  (package-install 'rustic))
(unless (package-installed-p 'tree-sitter)
  (package-install 'tree-sitter))
(unless (package-installed-p 'tree-sitter-langs)
  (package-install 'tree-sitter-langs))

(require 'rust-mode)
(require 'rustic)
(require 'tree-sitter)
(require 'tree-sitter-langs)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))

(setq rust-format-on-save t)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(add-hook 'rust-mode-hook 'lsp-deferred)

(provide 'init-rust)
;;; init-rust.el ends here
