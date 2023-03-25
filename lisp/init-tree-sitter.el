;;; init-rust.el --- Defaults for rust -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download tree-sitter
(unless (package-installed-p 'tree-sitter)
  (package-install 'tree-sitter))
(unless (package-installed-p 'tree-sitter-langs)
  (package-install 'tree-sitter-langs))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(tree-sitter-require 'cpp)
(tree-sitter-require 'rust)

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
