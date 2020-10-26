;;; redmine-mode.el --- use redmine from emacs
;;
;; Copyright (C) 2020 Erik Bäckman Author: Erik Bäckman <http://github/erikbackman>
;;
;; Author: Erik Bäckman <https://github.com/erikbackman>
;; Maintainer: Erik Bäckman <contact@ebackman.net>
;; Created: September 27 2020
;; Modified: September 27, 2020
;; Version: 0.0.1
;; Keywords: redmine
;; Homepage: https://github.com/erikbackman/emacs-redmine-mode
;; Package-Requires: ((emacs "25.1")
;;                    (cl-lib "0.5")
;;                    (dash "2.17.0")
;;                    (json "1.5")
;;                    (org "9.4-dev")
;;                    (request "0.3.2")
;;                    (s)
;;                    (hierarchy))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `redmine-mode' lets you work with redmine issues as org todo items.
;;
;;; Code:

(require 'dash)
(require 'json)
(require 'cl-lib)
(require 'org)
(require 'request)
(require 'map)
(require 'seq)
(require 's)
(require 'hierarchy)
(require 'redmine-mode-rest)
(require 'redmine-mode-util)
(require 'redmine-mode-org)
(require 'redmine-mode-project)

(defgroup redmine-mode nil
  "Redmine integration for Emacs and Org-mode"
  :prefix "redmine-mode-"
  :group 'redmine-mode)

(defcustom redmine-mode-api-key nil
  "Redmine API-key."
  :group 'redmine-mode
  :type 'string)

(defcustom redmine-mode-hostname nil
  "Redmine hostname."
  :group 'redmine-mode
  :type 'string)

(defvar redmine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd  "C-c C-c") #'redmine-put-issues)
    (define-key map (kbd  "C-c C-s") #'org-todo)
    (set-keymap-parent map org-mode-map)
    map))

;;;###autoload
(define-derived-mode redmine-mode org-mode "redmine"
  (setq org-todo-keywords
        '((sequence "NEW(n)" "INPROGRESS(p)" "RESOLVED(r)")))
  :keymap redmine-mode-map)

(defun redmine--get-children (issue other)
  "ISSUE OTHER."
  (seq-filter
   (lambda (i) (eq (lookup 'parent i) (lookup 'id issue)))
   other))

(defun build-issue-hierarchy (issues)
  "ISSUES."
  (hierarchy-from-list issues nil (lambda (x) (redmine--get-children x issues))))

;;;###autoload
(defun redmine-get-issues ()
  "Fetch Redmine issues and create an org buffer of todo items."
  (interactive)
  (let ((project-id (redmine--current-project-id)))

    (if (null project-id)
        (message "%s: %s" "Not a redmine project!" (redmine--current-project-dir))

      (when-let ((issues (redmine--get-issues project-id
                                              redmine-mode-hostname
                                              redmine-mode-api-key)))
        (let ((rmine-buf (get-buffer-create "*redmine-issues")))
          (with-current-buffer rmine-buf
            (let ((tree (build-issue-hierarchy issues)))
              (erase-buffer)
              (redmine-mode)

              (hierarchy-map
               (lambda (issue level)
                 (unless (hierarchy-has-root tree issue)
                   (insert (issue-as-todo issue level))
                   (forward-line)))
               tree)

              (switch-to-buffer rmine-buf))))))))

(defun issue-encode-for-put (issue)
  "ISSUE."
  (json-encode-plist
   `(:issue
     (:subject ,(plist-get issue :subject)
      :status_id ,(issue-state-to-status-id (plist-get issue :state))))))

(defun issue-state-to-status-id (state)
  "STATE."
  (pcase state
    ('"NEW" "1")
    ('"INPROGRESS" "2")
    ('"RESOLVED" "3")
    (_ "1")))

;;;###autoload
(defun redmine-put-issues ()
  "Doc."
  (interactive)
  (mapc
   (lambda (i) (put-issue (issue-encode-for-put i) redmine-mode-hostname redmine-mode-api-key))
   (read-buffer-todos "*redmine-issues*")))

(provide 'redmine-mode)
;;; redmine-mode.el ends here
