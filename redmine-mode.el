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
  :keymap redmine-mode-map
  )

;; UTIL
;;
(defun trim-ws (str)
  "STR."
  (replace-regexp-in-string "\s" "" str))

(defun lookup (k alist)
  "K ALIST."
  (cdr (assoc k alist)))

;; ORG
;;
;; (defun subtask? (issue)
;;   "ISSUE."
;;   (not (null (alist-get 'parent issue))))

;; Inline this
(defun todo-pattern (issue)
  "ISSUE."
  (if-let ((desc (alist-get 'description issue)))
      (concat "%s %s #%s: %s\n" desc "\n")
    "%s %s #%s: %s\n"))

(defun issue-as-todo (issue level)
  "ISSUE LEVEL."
  (format (todo-pattern issue)
          (s-repeat level "*")
          (trim-ws (upcase (alist-get 'status issue)))
          (alist-get 'id issue)
          (alist-get 'subject issue)))

(defun redmine-parse-todo (issue)
  "ISSUE."
  (let
      ((match
        (car
         (s-match-strings-all
          (rx "\#"
              (group (one-or-more digit))
              "\:"
              space
              (group (one-or-more any)))
          issue))))
    `(:id      ,(elt match 1)
      :subject ,(string-trim (elt match 2)))))

(defun redmine-entry-get (pom)
  "POM."
  (let ((e (redmine-parse-todo
            (org-entry-get pom "ITEM")))

        (s (org-entry-properties nil "TODO")))

    `(:id      ,(plist-get e :id)
      :subject ,(plist-get e :subject)
      :state   ,(lookup "TODO" s))))

(defun read-buffer-todos (buffer)
  "BUFFER."
  (with-current-buffer buffer
    (let (todos)
      (org-map-entries
       (lambda () (push (redmine-entry-get (point)) todos))
       nil)
      (nreverse todos))))

;; API
(defun parse-issue (issue)
  "ISSUE."
  (let-alist issue
    (if (and .id .subject)
        `((id          . ,.id)
          (status      . ,(alist-get 'name .status))
          (subject     . ,.subject)
          (description . ,(if (eq "" .description) nil .description))
          (parent      . ,(alist-get 'id .parent)))
      nil)))

(defun issue-encode-for-put (issue)
  "ISSUE."
  (json-encode-plist
   `(:issue
     (:subject ,(plist-get issue :subject)
      :status_id ,(issue-state-to-status-id (plist-get issue :state))))))

(defun put-issue (issue)
  "ISSUE."
  (request
    (format "http://%s/issues/%s.json?key=%s"
            redmine-mode-hostname
            (plist-get issue :id)
            redmine-mode-api-key)
    :type "PUT"
    :sync t
    :headers '(("Content-Type" . "application/json"))
    :data (issue-encode-for-put issue)
    :parser 'json-read
    :success (cl-function
              (lambda (&key _ &allow-other-keys)
                (message "Redmine PUT issues success")))))

(defun redmine--get-children (issue other)
  "ISSUE OTHER."
  (seq-filter
   (lambda (i) (eq (lookup 'parent i) (lookup 'id issue)))
   other))

(defun build-issue-hierarchy (issues)
  "ISSUES."
  (hierarchy-from-list issues nil (lambda (x) (redmine--get-children x issues))))

(defun redmine--get-issues (id)
  "ID."
  (let (json)
    (request
      (format "http://%s/issues.json?key=%s&project_id=%s"
              redmine-mode-hostname
              redmine-mode-api-key
              id)
      :sync t
      :parser 'json-read
      :headers '(("Content-Type" . "application/json"))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (message "%s" "Redmine GET issues success!")
                    (setq json data)))))
    (-map #'parse-issue (alist-get 'issues json))))

(defun redmine--current-project-dir ()
  "."
  (file-name-directory (buffer-file-name)))

(defun redmine--dirty-issues ()
  "."
  (let ((filename
         (concat (redmine--current-project-dir) ".redmine/dirty.json")))
    (with-temp-buffer
      (insert-file-contents filename)
      (json-parse-buffer :object-type 'alist
                         :array-type 'list))))

(defun redmine--current-project-id ()
  "."
  (let ((filename
         (concat (redmine--current-project-dir) "/.redmine/.redmine")))
    (with-temp-buffer
      (insert-file-contents filename)
      (let ((id (string-trim (buffer-string))))
        (if (string-empty-p id)
            nil
          t id)))))

;;;###autoload
(defun redmine-get-issues ()
  "Fetch Redmine issues and create an org buffer of todo items."
  (interactive)
  (let ((rmine-buf (get-buffer-create "*redmine-issues*"))
        (project-id (redmine--current-project-id)))

    (if (null project-id)
        (message "%s: %s" "Not a redmine project!" (redmine--current-project-dir))

      (with-current-buffer rmine-buf
        (let ((tree (build-issue-hierarchy (redmine--get-issues project-id))))
          (erase-buffer)
          (redmine-mode)

          (hierarchy-map
           (lambda (issue level)
             (unless (hierarchy-has-root tree issue)
               (insert (issue-as-todo issue level))
               (forward-line)))
           tree)

          (switch-to-buffer rmine-buf))))))

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
  (mapc #'put-issue (read-buffer-todos "*redmine-issues*")))

(provide 'redmine-mode)
;;; redmine-mode.el ends here
