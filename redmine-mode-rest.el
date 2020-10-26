;;; redmine-mode-rest.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: John Doe <http://github/ebn>
;; Maintainer: John Doe <john@doe.com>
;; Created: October 26, 2020
;; Modified: October 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/ebn/redmine-mode-rest
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
(require 'hierarchy)

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

(defun put-issue (issue host api-key)
  "ISSUE HOST API-KEY."
  (request
    (format "http://%s/issues/%s.json?key=%s"
            host
            (plist-get issue :id)
            api-key)
    :type "PUT"
    :sync t
    :headers '(("Content-Type" . "application/json"))
    :data issue
    :parser 'json-read
    :success (cl-function
              (lambda (&key _ &allow-other-keys)
                (message "Redmine PUT issues success")))))

(defun redmine--get-issues (id host api-key)
  "ID HOST API-KEY."
  (let (json)
    (request
      (format "http://%s/issues.json?key=%s&project_id=%s"
              host
              api-key
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

(provide 'redmine-mode-rest)
;;; redmine-mode-rest.el ends here
