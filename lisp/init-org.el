;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))

(maybe-require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))




(maybe-require-package 'writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  :init-value nil :lighter " Prose" :keymap nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))



;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))



;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))



(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")





(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'grab-mac-link)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (featurep (intern (concat "ob-" (symbol-name (car pair))))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t)))))

;; Copyright (C) 2021 Jake B <jakebox0@protonmail.com>

;; Author: Jake B <jakebox0@protonmail.com>
;; Original author of org-preview-html (until 2021-09): DarkSun <lujun9972@gmail.com>
;; Url: https://github.com/jakebox/org-preview-html
;; Keywords: Org, convenience, outlines
;; Version: 0.3
;; Package-Requires: ((emacs "25.1") (org "8.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This minor mode provides a side-by-side preview of your org-exported HTML
;; files using the either the eww or xwidget browsers. The update frequency of
;; the preview can be configured to suit your preference.
;;
;; Quick start:
;; Put this file under your load path.
;; Enable the minor mode in an Org buffer:
;;   M-x org-preview-html-mode
;; Configure options with M-x customize-group org-preview-html
;;
;; Source code
;; org-preview-html's code can be found here:
;;   http://github.com/jakebox/org-preview-html

;;; Code:

;;;; Requirements
(require 'org)
(require 'xwidget)
(require 'eww)


(defgroup org-preview-html nil
  "Automatically preview org-exported HTML files within Emacs."
  :group 'org-mode
  :link '(url-link :tag "Homepage" "https://github.com/jakebox/org-preview-html/"))

(defcustom org-preview-html-refresh-configuration 'save
  "Specifies how often the HTML preview will be refreshed.
  
If `manual', update manually by running `org-preview-html-refresh'.
If `save', update on save (default).
If `export', update on manual export \(using `org-html-export-to-html').
If `timer', update preview on timer (`org-preview-html-timer-interval').
If `instant', update ASAP (may cause slowdowns)."
  :type '(choice
		  (symbol :tag "Update preview manually"   manual)
		  (symbol :tag "Update preview on save"    save)
		  (symbol :tag "Update preview on export"  export)
		  (symbol :tag "Update preview on a timer" timer)
		  (symbol :tag "Update preview instantly"  instant))
  :group 'org-preview-html)

(defcustom org-preview-html-timer-interval 2
  "Integer seconds to wait between exports when in 'timer mode."
  :type 'integer
  :group 'org-preview-html)

(defcustom org-preview-html-viewer 'eww
  "Which Emacs browser `org-preview-html-mode' will use.

If `eww', use eww browser (default).
If `xwidget', use xwidget browser."
  :type '(choice
		  (symbol :tag "Use eww"      eww)
		  (symbol :tag "Use xwidget"  xwidget))
  :group 'org-preview-html)

(define-obsolete-variable-alias 'org-preview-html/body-only 'org-preview-html-subtree-only "Version 0.3")

(defcustom org-preview-html-subtree-only nil
  "If non-nil, scope the preview to the current subtree."
  :type 'boolean
  :group 'org-preview-html)

(defcustom org-preview-html/body-only nil
  "Scope the preview to the body or include the entire document.
Obselete as of version 0.3, instead use `org-preview-html-subtree-only'."
  :type 'boolean
  :group 'org-preview-html)


;; Internal variables
(defvar org-preview-html--browser-buffer nil)
(defvar org-preview-html--previewed-buffer-name nil)
(defvar org-preview-html--refresh-timer nil)
(defvar-local org-preview-html--html-file nil)


;; https://emacs.stackexchange.com/questions/7116/pop-a-window-into-a-frame
(defun org-preview-html-pop-window-to-frame ()
  "Pop a window to a frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;; Taken from frame.el Emacs 27.1, copied here for better version compatibility.
;; Without this here 27.1 required. With, 25.1.
(defun org-preview-html--previous-window-any-frame ()
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  0))
  (select-frame-set-input-focus (selected-frame)))


(defun org-preview-html-refresh ()
  "Exports the org file to HTML and refreshes the preview."
  ;; Refresh the preview.
  (interactive)
  ;; WIP If in manual mode it doesn't matter what buffer is active, just export and refresh
  (cond
   ((eq org-preview-html-refresh-configuration 'manual) ;; if in manual mode
		 (pop-to-buffer org-preview-html--previewed-buffer-name nil t)
		 (org-preview-html--org-export-html)
		 (org-preview-html--reload-preview))
		((unless (or (eq (eq (get-buffer org-preview-html--previewed-buffer-name) ;; TODO JAKE WHAT IS THIS
                             ;; In timer and instant modes the visible buffer matters
							 (window-buffer (selected-window))) nil)
					 (or (let ((state org-preview-html-refresh-configuration))
						   (eq state 'timer) (eq state 'instant))))
		   (org-preview-html--org-export-html)
		   (org-preview-html--reload-preview)))))

(defun org-preview-html--org-export-html ()
  "Silently export org to HTML."
  (let ((standard-output 'ignore))
	(org-export-to-file 'html org-preview-html--html-file
	  nil org-preview-html-subtree-only nil nil nil nil)))

(defun org-preview-html--reload-preview ()
  "Reload preview."
  (save-selected-window
	(pop-to-buffer org-preview-html--browser-buffer)
	(cond ((eq org-preview-html-viewer 'xwidget) (xwidget-webkit-reload))
		  ((eq org-preview-html-viewer 'eww)
		   (with-selected-window (selected-window)
			 ;; This stuff is to keep eww window scrolled at same point
			 (let ((eww-point (point))
				   (eww-window-start (window-start)))
			   (eww-reload)
			   (goto-char eww-point)
			   (set-window-start nil eww-window-start)))))))

(defun org-preview-html--kill-preview-buffer ()
  "Kill the xwidget preview buffer and pop back to the previewed org buffer."
  ;; Only do these things if the preview is around
  (when (bound-and-true-p org-preview-html--browser-buffer)
    ;; If preview is visible we first delete the window, otherwise
	;; just kill the preview buffer
	(if (get-buffer-window org-preview-html--browser-buffer 'visible)
		(delete-window (get-buffer-window org-preview-html--browser-buffer)))
	(let ((kill-buffer-query-functions nil))
	  (kill-buffer org-preview-html--browser-buffer))
	(pop-to-buffer org-preview-html--previewed-buffer-name)))

(defun org-preview-html--run-with-timer ()
  "Configure timer to refresh preview for `timer' mode."
  (setq org-preview-html--refresh-timer
		(run-at-time 1 org-preview-html-timer-interval #'org-preview-html-refresh)))

(defun org-preview-html--config ()
  "Configure buffer for preview: add exit hooks; configure refresh hooks."
  (setq org-preview-html--previewed-buffer-name (buffer-name))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Configure exit hooks
    (add-hook hook #'org-preview-html--stop-preview nil t))
  (let ((conf org-preview-html-refresh-configuration))
	(cond
	 ((eq conf 'manual))
	 ((eq conf 'save) ;; On save
	  (add-hook 'after-save-hook #'org-preview-html-refresh nil t))
	 ((eq conf 'timer) ;; every X seconds
	  (org-preview-html--run-with-timer))
	 ((eq conf 'export) ;; On export using org-html-export-html command manually
	  (advice-add 'org-html-export-to-html :after #'org-preview-html--reload-preview))
	 ((eq conf 'instant) ;; WIP Instantly (on self insert refresh)
	  (add-hook 'post-self-insert-hook #'org-preview-html-refresh nil t)))))

(defun org-preview-html--unconfig ()
  "Unconfigure 'org-preview-html-mode' (remove hooks and advice)."
  (let ((conf org-preview-html-refresh-configuration))
	(cond ((eq conf 'instant) ;; WIP
		   (remove-hook 'post-self-insert-hook #'org-preview-html-refresh t))
		  ((eq conf 'save)
		   (remove-hook 'after-save-hook #'org-preview-html-refresh t))
		  ((eq conf 'timer)
		   (cancel-timer org-preview-html--refresh-timer))
		  ((eq conf 'export)
		   (advice-remove 'org-html-export-to-html #'org-preview-html--reload-preview))))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Remove hooks
    (remove-hook hook #'org-preview-html--stop-preview t))
  ;; Reset variables
  (dolist (var '(org-preview-html--browser-buffer org-preview-html--previewed-buffer-name))
	(set var nil)))

(defun org-preview-html--open-browser ()
  "Open a browser to preview the exported HTML file."
  ;; Store the exported HTML filename
  (setq-local org-preview-html--html-file (concat (file-name-sans-extension buffer-file-name) ".html"))
  (unless (file-exists-p org-preview-html--html-file)
	(org-preview-html--org-export-html)) ;; Unless the file already exists, export it
  ;; Procedure to open the side-by-side preview
  (split-window-right)
  (other-window 1)
  (let ((file org-preview-html--html-file))
	(cond ((eq org-preview-html-viewer 'xwidget) (xwidget-webkit-browse-url (concat "file://" file)))
		  ((eq org-preview-html-viewer 'eww) (eww-open-file file))))
  (setq org-preview-html--browser-buffer (get-buffer (buffer-name)))
  (org-preview-html--previous-window-any-frame))

(defun org-preview-html--start-preview ()
  "Begin the org-preview-html preview."
  (when buffer-file-name
	(cond ((derived-mode-p 'org-mode)
		   (message "org-preview-html has recieved a major update - xwidgets support, refresh configurations and more! \n M-x customize-group org-preview-html-mode")
		   (org-preview-html--open-browser)
		   (org-preview-html--config))
		  (t
		   (org-preview-html-mode -1)
		   (user-error "`%s' not supported by org-preview-html preview, only `org mode'!" major-mode)))))

(defun org-preview-html--stop-preview ()
  "Stop the org-preview-html preview."
  (org-preview-html--kill-preview-buffer)
  (org-preview-html--unconfig))


;;;###autoload
(define-minor-mode org-preview-html-mode
  "(Optionally) live preview for Org exports to HTML."
  :lighter " org-preview-html"
  (if org-preview-html-mode
      (org-preview-html--start-preview)
    (org-preview-html--stop-preview)))

(provide 'init-org)
;;; init-org.el ends here
