;;; redmine-mode-org.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: John Doe <http://github/ebn>
;; Maintainer: John Doe <john@doe.com>
;; Created: October 26, 2020
;; Modified: October 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/ebn/redmine-mode-org
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
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
(require 'map)
(require 'seq)
(require 's)

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

(provide 'redmine-mode-org)
;;; redmine-mode-org.el ends here
