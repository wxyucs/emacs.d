;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))


;; Adjust garbage collection thresholds during startup, and thereafter

;;(let ((normal-gc-cons-threshold (* 20 1024 1024))
;;      (init-gc-cons-threshold (* 128 1024 1024)))
;;  (setq gc-cons-threshold init-gc-cons-threshold)
;;  (add-hook 'emacs-startup-hook
;;            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages

(require 'init-theme)
(require 'init-org)
(require 'init-git)
(require 'init-lsp)
(require 'init-evil)
(require 'init-treemacs)

(require 'init-cpp)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
