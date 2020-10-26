;;; redmine-mode-util.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: John Doe <http://github/ebn>
;; Maintainer: John Doe <john@doe.com>
;; Created: October 26, 2020
;; Modified: October 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/ebn/redmine-mode-util
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun trim-ws (str)
  "STR."
  (replace-regexp-in-string "\s" "" str))

(defun lookup (k alist)
  "K ALIST."
  (cdr (assoc k alist)))

(provide 'redmine-mode-util)
;;; redmine-mode-util.el ends here
