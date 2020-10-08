;;; rmine.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Erik Bäckman Author: Erik Bäckman <http://github/erikbackman>
;; Maintainer: Erik Bäckman Created: September 27, 2020 Modified: September 27, 2020 Version: 0.0.1
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
(require 'map)
(require 'seq)

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

(defvar redmine-mode-map (make-sparse-keymap))

(define-derived-mode redmine-mode org-mode "redmine"
  (setq org-todo-keywords
        '((sequence "NEW(n)" "INPROGRESS(p)" "RESOLVED(r)"))))

;; UTIL
;;
(defun trim-ws (str)
  "STR."
  (replace-regexp-in-string "\s" "" str))

(defun ws-to-us (str)
  "STR."
  (replace-regexp-in-string "\s" "_" str))

(defun concat-newline (s1 s2)
  "S1 S2."
  (concat s1 "\n" s2))

(defun lookup (k alist)
  "K ALIST."
  (cdr (assoc k alist)))

(defun try-lookup (k alist on-nil)
  "K ALIST ON-NIL."
  (if (nil? (lookup k alist))
      on-nil
    (lookup k alist)))

(defun alist-try-get (k alist on-nil)
  "K ALIST ON-NIL."
  (let ((val (alist-get k alist)))
    (if val val on-nil)))

(defun nil? (obj)
  "OBJ."
  (eq nil obj))

;; ORG
;;
(defun subtask? (issue)
  "ISSUE."
  (not (nil? (alist-get 'parent issue))))

(defun issue-as-todo (issue)
  "ISSUE."
  (format "* %s #%s: %s\n%s\n"
          (trim-ws (upcase (alist-get 'status issue)))
          (alist-get 'id issue)
          (alist-get 'subject issue)
          (try-lookup 'description issue "")))

(defun issue-as-sub-todo (issue)
  "ISSUE."
  (format "** %s #%s: %s\n%s"
          (trim-ws (upcase (alist-get 'status issue)))
          (alist-get 'id issue)
          (alist-get 'subject issue)
          (try-lookup 'desription issue "")))

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
          (description . ,.description)
          (parent      . ,(alist-get 'id .parent)))
      nil)))

(defun get-issues ()
  "."
  (let (json)
    (request
      (format "http://%s/issues.json?key=%s"
              redmine-mode-hostname
              redmine-mode-api-key)
      :sync t
      :parser 'json-read
      :headers '(("Content-Type" . "application/json"))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (setq json data)))))
    (-map #'parse-issue (alist-get 'issues json))))

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
                (message "Success")))))

(defun redmine-get-issues ()
  "Fetch Redmine issues and create an org buffer of todo items."
  (interactive)
  (let ((rmine-buf (get-buffer-create "*redmine-issues*")))
    (with-current-buffer rmine-buf
      (let ((tasks) (subtasks))
        (erase-buffer)
        (redmine-mode)

        (mapc (lambda (issue)
                (if (subtask? issue)
                    (push issue subtasks)
                  (push issue tasks)))
              (get-issues))

        (mapc (lambda (issue)
                (insert (issue-as-todo issue))
                (insert "\n")
                (forward-line))
              tasks)

        (mapc (lambda (issue)
                (message "%S" (alist-get 'parent issue))
                (org-goto--local-search-headings (concat "#" (pp (lookup 'parent issue))) nil nil)
                (search-forward-regexp (rx bol (* blank) eol))
                (insert (issue-as-sub-todo issue))
                (goto-char (point-max)))
              subtasks)
        (switch-to-buffer rmine-buf)))))

(defun issue-state-to-status-id (state)
  "STATE."
  (cond ((equal state "NEW")        "1")
        ((equal state "INPROGRESS") "2")
        ((equal state "RESOLVED")   "3")
        (t                          "1")))

(defun issue-encode-for-put (issue)
  "ISSUE."
  (json-encode-plist
   `(:issue
     (:subject ,(plist-get issue :subject)
      :status_id ,(issue-state-to-status-id (plist-get issue :state))))))

(defun rmine-sync-issues ()
  "Doc."
  (interactive)
  (let ((issues (--> (read-buffer-todos "*redmine-issues*")
                     (-map (lambda (x) x) it))))

    (mapc (lambda (i) (put-issue i)) issues)
  ))

(provide 'rmine)
;;; rmine.el ends here
