;;; redmine-mode-project.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: John Doe <http://github/ebn>
;; Maintainer: John Doe <john@doe.com>
;; Created: October 26, 2020
;; Modified: October 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/ebn/redmine-mode-project
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

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

(provide 'redmine-mode-project)
;;; redmine-mode-project.el ends here
