;;; prog-snippet.el -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Ricardo Iglesias <riglesia@stanford.edu>
;; URL
;; Keywords: convenience, prog-snippet, programming, reference
;; Package-Requires ((emacs "26.1") (emacsql "3.0.0")  (emacsql-sqlite3 "1.0.2"))
;;
;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This file defines the functions that can be called interactively.


(require 'prog-snippet-vars)            ;; Global variables
(require 'prog-snippet-db)              ;; Database interface
(require 'prog-snippet-buffer)          ;; UI
(require 'prog-snippet-file)            ;; File info
(require 'prog-snippet-org)             ;; Org-mode interface
(require 'prog-snippet-search)          ;; Querying logic. 

(defun prog-snippet--initialize ()
  "Initializes the database, as well as variables for other parts of the program."
  (interactive)
  (prog-snippet-db--initialize)
  (prog-snippet-org--initialize-keymap)
  (prog-snippet-org--initialize-link-type))

(defun prog-snippet--create-file (snippet-title primary-tag tags &optional db)
  (interactive "sTitle: \nsPrimary Tag: \nsTags (space-delimited): ")
  (let ((file-info (prog-snippet-file--init snippet-title
                                            primary-tag
                                            (split-string tags))))
    (unless db (setq db (prog-snippet-db--get-connection)))
    (prog-snippet-db--create-new-file db file-info)
    (prog-snippet-org--create-new-file file-info)))

(defun prog-snippet--delete-file (filename)
  "Deletes a file and removes it from the database."
  (let ((id (prog-snippet-org--get-id filename)))
    (delete-file filename)
    (prog-snippet-db--delete-file (prog-snippet-db--get-connection) id)))

(defun prog-snippet--modify-title (new-title)
  "Modifies the title of the current file. "
  (interactive "sNew Title: ")
  (let ((id (prog-snippet-org--get-id (buffer-file-name)))
        (new-path (prog-snippet-file--create-filename new-title)))
    (prog-snippet-db--modify-title
     (prog-snippet-db--get-connection)
     id
     new-title
     new-path)
    (prog-snippet-org--modify-title new-path new-title)))

(defun prog-snippet--basic-search (tags)
  "Performs a search for a space-delimited set of tags. Any file
in the results must have all of the tags specified by the user."
  (interactive "sTags (Space-Delimited): ")
  (let* ((query-input (prog-snippet-search--generate-query-input tags))
         (query-result (prog-snippet-search--query-from-string tags)))
    (prog-snippet-buffer--draw-query-results query-input query-result)))

(defun prog-snippet--full-search (primary intersect union)
  "Performs a search for union, intersection, and primary tags.  "
  (interactive "sEnter Primary Tag: \nsEnter Intersection Tags (Space-Delimited): \nsEnter Union Tags(Space-Delimited): ")
  (let* ((query-input (prog-snippet-search--generate-query-input intersect primary union))
         (query-result (prog-snippet-search--query query-input)))
    (prog-snippet-buffer--draw-query-results query-input query-result)))

(defun prog-snippet--get-file-tags ()
  "Displays the set of tags a current file has. Must be inside a
file tracked by the database."
  (interactive)
  (let* ((id (prog-snippet-org--get-id (buffer-file-name)))
         (db (prog-snippet-db--get-connection))
         (tags (prog-snippet-db--get-tags-in-file db id)))
    (message "(%d tag%s):  %s "
             (length tags)
             (if (= 1 (length tags)) "" "s")
             (string-join tags ", "))))

(defun prog-snippet--insert-tags (new-tags)
  "Inserts a new tag into the current file. Must be inside a file tracked by the database."
  (interactive "sEnter list of tags (Space-Delimited): ")
  (let* ((id (prog-snippet-org--get-id (buffer-file-name)))
         (db (prog-snippet-db--get-connection))
         (current-tags (prog-snippet-db--get-tags-in-file db id))
         (to-insert
          (--> (split-string new-tags)
               (mapcar (lambda (s) (downcase (string-trim s)))
                       it)
               (seq-filter (lambda (new-tag) (not (cl-find new-tag current-tags
                                                           :test 'equal)))
                           it)
               (delete-duplicates it :test 'string-equal))))
    (dolist (new-tag to-insert)
      (prog-snippet-db--insert-one-tag db new-tag id))))

(defun prog-snippet--reset ()
  "Deletes the database and sets the connection to nil. 

This is a destructive operation. There is no backup available to
you if you delete the database."
  (interactive)
  (prog-snippet-db--delete-db prog-snippet-db--connection prog-snippet-db-location)
  (setq prog-snippet-db--connection nil))

;;; TODO: Improve export function to not include #+TITLE all over the place. 
(defun prog-snippet--export-query-results (tag-string)
  "Queries the database with a set of space-delimited tags and
joins the contents of the matches in a single file."
  (interactive "sTags (Space-Delimited): ")
  (let ((query-results (prog-snippet--query-from-string tag-string)))
    (dolist (row query-results)
      (let ((path (fourth row)))
        (find-file path)
        (write-region nil nil "export.org" t))))
  (find-file "export.org"))

;;; TODO: Create a way to build the database from a set of files.
;;; I might need to add a #+TAGS in the org-mode buffer, then find a way to keep
;;; this updated.


(provide 'prog-snippet)
