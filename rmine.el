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

;; UTIL
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

(define-derived-mode redmine-mode org-mode "redmine")
(setq sample-results (json-read-file "issues.json"))

(defun issue-as-todo (issue)
  "ISSUE."
  (format "** TODO ISSUE: %s - %s\n%s"
          (lookup 'id issue)
          (lookup 'subject issue)
          (try-lookup 'description issue "")))

(defun parse-issue (issue)
  "ISSUE."
  (let-alist issue
    (if (and .id .subject)
        `((id          . ,.id)
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

(defun rmine-get-issues ()
  "Fetch Redmine issues and create an org buffer of todo items."
  (interactive)
  (let ((rmine-buf (get-buffer-create "*test*")))
    (with-current-buffer rmine-buf
      (erase-buffer)
      ;; (insert "* ISSUES\n")
      (insert (format-issues sample-results))
      (redmine-mode)
      (switch-to-buffer-other-window rmine-buf))))

(defun rmine-issue-to-json-string (issue)
  "ISSUE."
  (format "{ \"issue\": { \"subject\": %s, \"status_id\": %s } }"
          (plist-get issue :subject)
          (plist-get issue :state)))

;; just format and print the json structure for now
;; TODO: post each issue
;; PUT /issues/[id].json
;; {
;;   "issue": {
;;     "subject": "Subject changed",
;;     "notes": "The subject was changed"
;;   }
;; }
(defun rmine-sync-issues ()
  "Doc."
  (interactive)
  (--> (read-buffer-todos "*test*")
       (-reduce-r-from (lambda (x z)
                         (format "%s,\n%s" (rmine-issue-to-json-string x) z))
                       ""
                       it)
       (concat "[" it "]")
       (message "%S" it)))

;; ORG
(defun redmine-parse-issue (issue)
  "ISSUE."
  (let
      ((match
        (car (s-match-strings-all
              (rx "ISSUE\:"
                  space
                  (group (one-or-more digit))
                  space
                  "-"
                  space
                  (group (one-or-more (or word space digit))))
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

(provide 'rmine)
;;; rmine.el ends here
