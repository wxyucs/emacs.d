;;; init-markdown.el --- Defaults for markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download markdown-mode
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(unless (package-installed-p 'impatient-mode)
  (package-install 'impatient-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("README" . markdown-mode))

(defun markdown-html (buffer)
    (princ (with-current-buffer buffer
	(format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))
(defun markdown-custom ()
  "markdown-mode-hook"
  (impatient-mode 1)
  (setq httpd-host "0.0.0.0")
  (setq imp-user-filter #'markdown-html))
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))

(provide 'init-markdown)
;;; init-markdown.el ends here
