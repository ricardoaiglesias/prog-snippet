;;; prog-snippet-file.el -*- coding: utf-8; lexical-binding: t; -*-
;;;
;; Author: Ricardo Iglesias <riglesia@stanford.edu>
;; URL
;; Keywords: convenience, prog-snippet, programming, reference
;; Package-Requires ((emacs "26.1"))
;;
;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This file defines the "prog-snippet-file" class, which is essentially just a
;; wrapper around several pieces of information required in a prog-snippet file. 

(require 'prog-snippet-vars)

(defclass prog-snippet-file ()
  ((title :initarg :title)
   (snippet-id :initarg :id)
   (primary-tag :initarg :ptag)
   (tag-list :initarg :tags)
   (path :initarg :path)))

(defun prog-snippet-file--create-filename (snippet-title)
  (cl-flet ((replace-ws (s) (replace-regexp-in-string "[[:space:]]\\|-\\|/" "_" s)))
    (format "%s_%s.org"
            (replace-ws snippet-title)
            (format-time-string "%Y-%m-%d-%H:%M:%S"))))

;; Original Source ID: 90aebf38-b33a-314b-1198-c9bffea2f2a2
;; Modified for a different-formatted output, and an easier way to add-remove
;; functions to use in the md5 hash.
(defun prog-snippet-file--uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let* ((functions (list 'user-uid 'emacs-pid 'system-name 'user-full-name
                          'current-time 'emacs-uptime 'garbage-collect 'random
                          'recent-keys))
         (s (--> (mapconcat (lambda (fn) (format "%s" (funcall fn)))
                            functions "")
                 (md5 it)))
         (spacing 8)
         (md5-len (- (length s) 1)))
    (--> ;; Generate pairs of indices given the spacing.
         (mapcar (lambda (x) (list x (min (+ x spacing) md5-len)))
                 (number-sequence 0 md5-len spacing))
         ;; Generate the substrings.
         (mapcar (lambda (pair) (pcase-let ((`(,start ,end) pair))
                                  (substring s start end)))
                 it)
         (string-join it "."))))

(defun prog-snippet-file--init (input-title input-primary-tag input-tags
                                            &optional folder)
  (unless folder (setq folder prog-snippet-dir))
  (let* ((filename (prog-snippet-file--create-filename input-title))
         (path (expand-file-name filename folder))
         (primary-tag (string-trim (downcase input-primary-tag)))
         (tags (--> (mapcar (lambda (s) (string-trim (downcase s))) input-tags)
                    (append (list primary-tag) it)
                    (delete-dups it)))
         (id (prog-snippet-file--uuid-create)))
    (make-instance 'prog-snippet-file
                   :title input-title
                   :id id
                   :ptag primary-tag
                   :tags tags
                   :path path)))

(provide 'prog-snippet-file)
