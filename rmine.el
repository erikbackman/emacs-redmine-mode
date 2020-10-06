;;; rmine.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Erik Bäckman Author: Erik Bäckman <http://github/erikbackman>
;; Maintainer: Erik Bäckman
;; Created: September 27, 2020
;; Modified: September 27, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/erikbackman/rmine.el
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5") (dash 2.17.0) (json) (org))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'dash)
(require 'json)
(require 'cl-lib)
(require 'org)
(require 'request)

;; CONFIG
;;
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

;; UTIL
;;
(defun trim-ws (str)
  "STR."
  (replace-regexp-in-string "\s" "" str))

(defun ws-to-us (str)
  "STR."
  (replace-regexp-in-string "\s" "_" str))

(defun nil? (x)
  "X."
  (eq nil x))

(defun concat-newline (s1 s2)
  "S1 S2."
  (concat s1 "\n" s2))

(defun lookup (k alist)
  "K ALIST."
  (cdr (assoc k alist)))

(defun try-lookup (k alist on-nil)
  "K ALIST ON-NIL."
  (let ((val (lookup k alist)))
    (if (nil? val)
        on-nil
      val)))

;; REDMINE
(defvar sample-results nil)

(defvar redmine-mode-map (make-sparse-keymap))
(define-derived-mode redmine-mode org-mode "redmine")
(setq sample-results (json-read-file "issues.json"))

(defun issue-as-todo (issue)
  "ISSUE."
  (format "** %s #%s: %s\n%s\n"
          (ws-to-us (upcase (lookup 'status issue)))
          (lookup 'id issue)
          (lookup 'subject issue)
          (try-lookup 'description issue "")))

(defun parse-issue (issue)
  "ISSUE."
  (let-alist issue
    (if (and .id .subject)
        `((id          . ,.id)
          (status      . ,(lookup 'name .status))
          (subject     . ,.subject)
          (description . ,.description))
      nil)))

(defun format-issues (issues)
  "ISSUES."
  (--> (lookup 'issues issues)
       (-map #'parse-issue it)
       (-remove #'nil? it)
       (-map #'issue-as-todo it)
       (-reduce-r #'concat-newline it)))



;; ORG
(defun redmine-parse-issue (issue)
  "ISSUE."
  (let
      ((match
        (car
         (s-match-strings-all
          (rx "\#"
              (group (one-or-more digit))
              "\:"
              space
              (group (one-or-more (or word space "-" digit))))
          issue))))
    `(:id      ,(elt match 1)
      :subject ,(string-trim (elt match 2)))))

(defun redmine-entry-get (pom)
  "POM."
  (let ((e (redmine-parse-issue
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
;; (-map #'json-encode-plist (read-buffer-todos "*test*"))
;; (json-read-from-string "{\"id\":\"123\",\"subject\":\"do the thing\",\"state\":\"TODO\"}")

;; API
(defun get-issues ()
  "."
  (let (json)
    (request
      (format "http://%s/issues.json?key=%s" redmine-mode-hostname redmine-mode-api-key)
      :sync t
      :parser 'json-read
      :headers '(("Content-Type" . "application/json"))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (setq json data)))))
    (format-issues json)))

(defun post-issue (issue)
  "ISSUE."
  (request
    (format "http://%s/issues/%s.json?key=%s" redmine-mode-hostname (plist-get issue :id) redmine-mode-api-key)
    :type "PUT"
    :sync t
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode-plist (rmine-issue-to-json issue))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "I sent: %S" (assoc-default 'json data))))))

(defun rmine-get-issues ()
  "Fetch Redmine issues and create an org buffer of todo items."
  (interactive)
  (let ((rmine-buf (get-buffer-create "*test*")))
    (with-current-buffer rmine-buf
      (setq org-todo-keywords
                  '((sequence "NEW" "IN_PROGRESS" "RESOLVED")))
      (erase-buffer)
      (insert (get-issues))
      (redmine-mode)
      (switch-to-buffer rmine-buf))))

(defun rmine-issue-to-json-string (issue)
  "ISSUE."
  (format "{ \"issue\": { \"subject\": %s, \"status_id\": %s } }"
          (plist-get issue :subject)
          (plist-get issue :state)))

(defun issue-state-to-status-id (state)
  "STATE."
  (cond ((equal state "NEW") "1")
        ((equal state "IN_PROGRESS") "2")
        ((equal state "RESOLVED") "3")
        (t "1")))

(defun rmine-issue-to-json (issue)
  "ISSUE."
  `(:issue
    (:subject ,(plist-get issue :subject)
     :status_id ,(issue-state-to-status-id (plist-get issue :state)))))

(defun rmine-sync-issues ()
  "Doc."
  (interactive)
  (let ((issues (--> (read-buffer-todos "*test*")
                     (-map (lambda (x) x) it))))

    (mapc (lambda (i) (post-issue i)) issues)
  ))

(provide 'rmine)
;;; rmine.el ends here
