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

(defvar sample-results nil)
(setq sample-results (json-read-file "issues.json"))

(defun lookup (k alist)
  "K ALIST."
  (cdr (assoc k alist)))

(defun show-issue (issue)
  "ISSUE."
  (format "Issue: %s - %s"
          (lookup 'id issue)
          (lookup 'subject issue)))

(defun parse-issue (issue)
  "ISSUE."
  `((id      . ,(lookup 'id issue))
    (subject . ,(lookup 'subject issue))))

(defun format-todo-issues (issues)
  "ISSUES."
  (--reduce-r-from (concat (concat "** TODO " it) "\n" acc) "" issues))

(defun format-issues (issues)
  "ISSUES."
  (--> (lookup 'issues issues)
       (-map #'parse-issue it)
       (-map #'show-issue it)))

(defun rmine-get-issues ()
  "Fetch Redmine issues and create an org buffer of todo items."
  (interactive)
  (let ((rmine-buf (get-buffer-create "*test*")))
    (with-current-buffer rmine-buf
    (erase-buffer)
    (insert "* ISSUES\n")
    (insert (format-todo-issues (format-issues sample-results)))
    (org-mode)
    (switch-to-buffer-other-window "*test*"))))

(provide 'rmine)
;;; rmine.el ends here
