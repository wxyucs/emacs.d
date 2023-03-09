;;; init-lsp.el --- Defaults for lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download lsp-mode
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))

;; Download lsp-ui
(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))

;; Enable lsp-mode
(require 'lsp-mode)
(lsp-mode 1)

;; Enable lsp-ui
(require 'lsp-ui)

(setq lsp-ui-mode t

      lsp-ui-sideline-enable            t
      lsp-ui-sideline-show-diagnostics  t
      lsp-ui-sideline-show-hover        nil
      lsp-ui-sideline-show-code-actions t
      lsp-ui-sideline-update-mode       'point
      lsp-ui-sideline-delay             1

      lsp-ui-peek-enable nil

      lsp-ui-doc-enable                 t
      lsp-ui-doc-position               'at-point
      lsp-ui-doc-delay                  1
      lsp-ui-doc-show-with-cursor       t

      lsp-ui-imenu-enable nil
      )

(provide 'init-lsp)
;;; init-lsp.el ends here
