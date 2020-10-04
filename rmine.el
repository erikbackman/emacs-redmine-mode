;;; rmine.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Erik Bäckman
;;
;; Author: Erik Bäckman <http://github/erikbackman>
;; Maintainer: Erik Bäckman
;; Created: September 27, 2020
;; Modified: September 27, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/erikbackman/rmine.el
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5") (dash 2.17.0) (json))
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
(require 'rmine-org)
(require 'rmine-util)

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
    (if (and .id .subject .description)
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

(provide 'rmine)
;;; rmine.el ends here
