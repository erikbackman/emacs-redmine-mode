;;; rmine-org.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Erik Bäckman
;;
;; Author: Erik Bäckman <http://github/eb>
;; Maintainer: Erik Bäckman <eb@solus>
;; Created: October 04, 2020
;; Modified: October 04, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/eb/rmine-org
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'dash)
(require 'json)

(defun redmine-parse-issue (issue)
  "ISSUE."
  (let ((match
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


(provide 'rmine-org)
;;; rmine-org.el ends here
