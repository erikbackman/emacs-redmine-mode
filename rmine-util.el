;;; rmine-util.el  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Erik Bäckman
;;
;; Author: Erik Bäckman <http://github/eb>
;; Maintainer: Erik Bäckman <eb@solus>
;; Created: October 04, 2020
;; Modified: October 04, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/eb/rmine-util
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

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

(provide 'rmine-util)
;;; rmine-util.el ends here
