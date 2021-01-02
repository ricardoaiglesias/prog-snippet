;;; prog-snippet-snippet.el -*- coding: utf-8; lexical-binding: t; -*-
;; Author: Ricardo Iglesias <riglesia@stanford.edu>
;; URL
;; Keywords: convenience, prog-snippet, programming, reference
;; Package-Requires ((emacs "26.1"))
;;
;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This file provides a way to search through the database given a set of search
;; strings.

(require 'prog-snippet-db)

(defun prog-snippet-search-dispatch (db primary-tag union-tags intersect-tags)
  (cl-flet ((is-empty? (l) (= (length l) 0)))
    (let ((empty-union (is-empty? union-tags))
          (empty-intersect (is-empty? intersect-tags)))
      (cond ((and empty-union empty-intersect (null primary-tag))
             (user-error "Unable to search without terms. (Union %s, Intersect %s, Primary %s)"
                         union-tags intersect-tags primary-tag))
            (t (prog-snippet-db--search
                db union-tags intersect-tags primary-tag))))))

(defun prog-snippet-search--sanitize-string (in-string tags)
  (--> (downcase in-string)
       (split-string it)
       (cl-remove-duplicates it :test #'equal)
       (seq-filter (lambda (to-search) (cl-find to-search tags :test 'equal)) it)
       (mapcar (lambda (s) (downcase s)) it)))

(defun prog-snippet-search--alist-to-lists (snippet-alist tags)
  (cl-flet* ((get-tags
              (key) (--> (assoc key snippet-alist)
                         (cdr it)
                         (if it (prog-snippet-search--sanitize-string it tags)
                           '("")))))
    (mapcar (lambda (key)  (cons key (get-tags key)))
            '(:union :intersect :primary))))

(defun prog-snippet-search--query (snippet-alist &optional db)
  "Given an assoc-list of symbols to a space-separated string of
tags, query the database appropriately.

The `db' parameter can be optionally specified to use a custom
database with the appropriate schema."
  (unless db (setq db (prog-snippet-db--get-connection)))
  (let* ((db-tags (prog-snippet-db--get-tags db))
         (tag-lists (prog-snippet-search--alist-to-lists snippet-alist db-tags))
         (primary-tag (first (cdr (assoc :primary tag-lists))))
         (unions (cdr (assoc :union tag-lists)))
         (intersects (cdr (assoc :intersect tag-lists))))
    (prog-snippet-search-dispatch db primary-tag unions intersects)))

(defun prog-snippet-search--generate-query-input (intersect &optional primary union)
  (list (cons :primary (if primary primary ""))
        (cons :intersect intersect)
        (cons :union (if union union ""))))

(defun prog-snippet-search--query-from-string (tag)
  (let ((input (prog-snippet--generate-query-input tag)))
    (prog-snippet-search--query input)))

(provide 'prog-snippet-search)
