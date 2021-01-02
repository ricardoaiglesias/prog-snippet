;;; snippet-db.el -*- coding: utf-8; lexical-binding: t; -*-
;;;
;;;

;; Author: Ricardo Iglesias <riglesia@stanford.edu>
;; URL
;; Keywords: convenience, prog-snippet, programming, reference
;; Package-Requires ((emacs "26.1") (emacsql "3.0.0")  (emacsql-sqlite3 "1.0.2"))
;;
;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This library provides an underlying database API to prog-snippet.

(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'dash)
(require 'cl-seq)

(require 'prog-snippet-vars)
(require 'prog-snippet-file)

;;;; Options
(defcustom prog-snippet-db-name "prog-snippet.sqlite"
  "Directory where the prog-snippet database lives."
  :type 'string :group 'prog-snippet)

(defcustom prog-snippet-db-location
  (expand-file-name "prog-snippet.db" prog-snippet-dir)
  "Full path for the snippet database."
  :type 'string
  :group 'prog-snippet)

(defvar prog-snippet-db--connection nil "Database connection to the prog-snippet database.")
;;;; Database API

;;; Schema
(defconst prog-snippet-table-schema
  '((files [(file-id :unique)
            (path :primary-key)
            (title :not-null)
            (primary-tag :not-null)
            (tag-list)])
    ;; Each file ID has multiple tags
    (tags [(file-id)
           (tag)]))
  "The schema for the tables used within the prog-snippet
  environment. Below is an explanation for each table:

files: Contains the path for a given snippet. The path is
relative to prog-snippet-dir. The `file-id' field is a
randomly-generated UUID. Additionally, it contains a \"primary
tag\" column, which allows for coarse filtering of files based on
general tags (such as \"C++\" or \"Rust\") and a `tag-list',
which is simply a list of hte available tags.

tags: A one-to-many mapping from a file ID to a list of tags that
correspond to the file. There is some redundancy between the tags
and the files table, but this redundancy allows for different
ways of querying. ")

;;;;; Initialization
(defun prog-snippet-db-initialized-p ()
  "Checks whether the prog-snippet database has been initialized."
  (and (file-exists-p prog-snippet-db-location) ;; Ensure folder exists.
       prog-snippet-db--connection)) ;; Ensure db isn't null

(defun prog-snippet-db--ensure-initialized (db)
  (unless  (emacsql-live-p db)
    (user-error "[prog-snippet] Your database isn't built yet. Please run prog-snippet-db-build-db.")))

(defun prog-snippet-db--create (db-name)
  (when (not (file-exists-p prog-snippet-dir)) (mkdir prog-snippet-dir))
  (setq prog-snippet-db--connection (emacsql-sqlite3 db-name)))

(defun prog-snippet-db--initialize-new-db ()
  "This function initializes the database connection when no "
  (prog-snippet-db--create prog-snippet-db-name)
  (prog-snippet-db--create-tables (prog-snippet-db--get-connection)))

(defun prog-snippet-db--initialize ()
  (cond ;; Do nothing when already initialized
         (prog-snippet-db--connection prog-snippet-db--connection)
         ;; When db is created but variable is still nil.
         ((file-exists-p prog-snippet-db-location) (prog-snippet-db--open-db))
         ;; Otherwise, db hasn't been created yet.
         (t
          (progn (prog-snippet-db--open-db)
                 (prog-snippet-db--create-tables
                  (prog-snippet-db--get-connection))))))

(defun prog-snippet-db--open-db ()
  (setq prog-snippet-db--connection
                (emacsql-sqlite3 prog-snippet-db-location)))

(defun prog-snippet-db--create-tables (db)
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) prog-snippet-table-schema)
      (emacsql db [:create-table $i1 $S2] table schema))))

(defun prog-snippet-db--get-connection ()
  "Returns the active SQLite connection."
  (unless prog-snippet-db--connection (user-error "Uninitialized database. Please run 'prog-snippet-init"))
  prog-snippet-db--connection)

(defun prog-snippet-db--get-temp-db (name)
  (emacsql-sqlite3 name))

;;; Insertion
(defun prog-snippet-db--create-new-file (db file-info)
  (cl-flet ((db-insert--tag (db id tag)
                            (emacsql db [:insert :into tags :values ([$s1 $s2])]
                                     id tag))
            (db-insert--file
             (db title primary-tag id filename tags)
             (emacsql db [:insert :into files :values $v1]
                      `[,id ,filename ,title ,primary-tag ,tags ])))
    (let ((title (oref file-info title))
          (id (oref file-info snippet-id))
          (primary (oref file-info  primary-tag))
          (tags (oref file-info tag-list))
          (path (oref file-info path)))

      ;; Create new entries for each of the tables.
      (db-insert--file db title primary id path tags)
      (dolist (tag tags) (db-insert--tag db id tag)))))

(defun prog-snippet-db--insert-one-tag (db tag id)
  (emacsql db [:insert :into tags :values [$s1 $s2]] id tag))

;;; Deletion
(defun prog-snippet-db--delete-one-tag (db tag id)
  (emacsql db [:delete :from tags
               :where (= tags:file-id $s1) :and (= tags:tag tag)] id tag))

(defun prog-snippet-db--delete-file (db id)
  (emacsql-with-transaction db
    (emacsql db [:delete :from files :where (= file-id $s1)] id)
    (emacsql db [:delete :from tags :where (= file-id $s1)] id)))

;;; Modification
(defun prog-snippet-db--modify-title (db snippet-id new-title new-path)
  (emacsql db [:update files
               :set [(= title $s1)
                     (= path $s2)]
               :where (= file-id $s3)] new-title new-path snippet-id))

;; (defun prog-snippet-create-db-test-db ()
;;   "Creates the test database for the 'test-prog-snippet-db' tests."
;;   (let* ((temp-name (concat (make-temp-name "prog-snippet-test-") ".sqlite"))
;;          (db (prog-snippet-db--get-temp-db temp-name)))
;;     (prog-snippet-db--create-tables db)
;;     (prog-snippet-db--batch-insert
;;      db
;;      '("File A" "C++" ("vector" "map" "stl" "iterator"))
;;      '("File B" "Elisp" ("macro" "map" "quote"))
;;      '("File C" "Elisp" ("macro" "threading" "dash"))
;;      '("File D" "Rust" ("tuple" "structs"))
;;      '("File E" "Rust" ("ownership" "borrow" "mutable"))
;;      '("File F" "Python" ("range" "list"))
;;      '("File G" "Python" ("numpy" "data-analysis"))
;;      '("File H" "Elisp" ("vector" "map" "stl" "iterator"))
;;      '("File I" "C++" ("map" "iterator" "const")))
;;     db))

;;; Querying
(defun interleave (lst separator)
  "Interleave `separator' between elements of lst.
Source: https://github.com/org-roam/org-roam/blob/f2976fa3be7dddd02f07b5d8367b1742ed27c79d/org-roam-macs.el#L47"
  (when lst
    (let ((new-lst (list (pop lst))))
      (dolist (it lst)
        (nconc new-lst (list separator it)))
      new-lst)))

(defun tag-filter (predicate tag-list)
  "Converts a list into a list of functions, separated by the
symbol `:or'.

EXAMPLE:

(tag-filter (lambda (x) (= x 5)) '(1 2 3))
Result:
(list (lambda (x) (= 1 5)) :or (lambda (x) (= 2 5)) :or (lambda (x) (= 3 5)))
 "
  (--> tag-list (mapcar predicate it) (interleave it :or)))

(defun tag-union (tag-list)
  "Returns a list of conditions for a query for a file that
contains at least one tag in `taglist'"
  (if (equal tag-list '()) (list (list '= 'true 'true))
    (tag-filter (lambda (tag-string) (list '= 'tags:tag tag-string))
                tag-list)))

(defun prog-snippet-db--get-tags (db &optional primary-tag)
  "Returns the list of tags currently stored in the database. "
  (let ((has-primary? (if primary-tag (list '= 'files:primary-tag primary-tag)
                        (list '= 'true 'true))))
    ;;; Flatten the list of tags. 
    (apply #'append
           (emacsql db `[:select :distinct [tag]
                         :from tags
                         :join files
                         :on (= tags:file-id files:file-id) :and ,has-primary?]))))

(defun prog-snippet-db--get-tags-in-file (db file-id)
  "Given a `file-id', returns the tags that correspond to that file."
  (apply #'append
         (emacsql db [:select :distinct [tags:tag]
                      :from tags
                      :where (= tags:file-id $s1)] file-id)))

(defun prog-snippet-db--get-files-with (db tags &optional primary-tag)
  "Returns files in the database that have ANY of the tags in `tags'."
  (let* ((has-primary? (if primary-tag
                           (list '= 'files:primary-tag primary-tag)
                         (list '= 'true 'true)))
         (satisfies-union-condition (tag-union tags)))
    (emacsql db `[:select :distinct
                  [files:file-id files:tag-list files:title files:path files:primary-tag]

                  :from (as [:select file-id
                             :from tags
                             :where ,@satisfies-union-condition] union-table)
                  :join files
                  :on (= union-table:file-id files:file-id)
                  :and ,has-primary?])))

(defun prog-snippet-db--search (db union-tags intersect-tags &optional primary-tag)
  "Queries the prog-snippet database `db'.

`intersect-tags' specifies a list of tags that MUST be in any
file returned from the query.
All files in the query must have at least one tag found in `union-tags'.

`primary-tag' Allows a coarse filter over the results."
  (unless (and (listp union-tags) (listp intersect-tags))
    (signal 'wrong-type-argument (list 'listp union-tags intersect-tags)))

  (let* ((allow-list (remove-duplicates (append union-tags intersect-tags)))
         (candidates (prog-snippet-db--get-files-with db allow-list primary-tag)))
    (seq-filter
     (lambda (row) (let ((file-tags (second row)))
                     (cl-subsetp intersect-tags file-tags :test 'equal)))
     candidates)))

(defun prog-snippet-db--dump-files (&optional db)
  (interactive)
  (unless db (setq db (prog-snippet-db--get-connection)))
  (insert (format "%s" (emacsql db [:select * :from files]))))

;;;  Helper methods for testing.
(defun prog-snippet-db--batch-insert (db &rest data)
  "Inserts many files at once. Useful for testing. 
USAGE:

(prog-snippet--batch-insert
          (prog-snippet-db--get-connection)
          '(\"File A\" \"C++\" (\"vector\" \"map\" \"stl\" \"iterator\"))
          '(\"File B\" \"Rust\" (\"vector\" \"map\" \"stl\" \"iterator\"))
          '(\"File C\" \"Rust\" (\"vector\" \"iterator\"))
          '(\"File D\" \"ELisp\" (\"quote\" \"map\" \"cons\" \"iterator\")))"
  (pcase-dolist (`(,filename ,primary-tag ,tags) data)
    (prog-snippet-db--create-new-file
     db (prog-snippet-file--init filename primary-tag tags))))

(defmacro prog-snippet--with-temp-db! (&rest forms)
  (let ((temp-name (make-symbol "temp-name"))
        (temp-result (make-symbol "temp-result")))
    `(let* ((,temp-name (make-temp-name "prog-snippet-temp-"))
            (temp-db (prog-snippet-db--get-temp-db ,temp-name)))
       (prog-snippet-db--create-tables temp-db)
       (let ((,temp-result (progn ,@forms)))
          (prog-snippet-db--delete-tables temp-db)
          (delete-file ,temp-name)
          ,temp-result))))

;; Close database / Connection
(defun prog-snippet-db--delete-db (db name)
  (prog-snippet-db--delete-tables db)
  (delete-file name))

(defun prog-snippet-db--close-connection (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `prog-snippet-dir'."
  (unless db (setq db (prog-snippet-db--get-connection)))
  (when (and db (emacsql-live-p db)) (emacsql-close db)))

(defun prog-snippet-db--delete-tables (&optional db)
  (unless db (setq db (prog-snippet-db--get-connection)))
  (when (and db (emacsql-live-p db))
    (pcase-dolist (`(,table . ,_schema) prog-snippet-table-schema)
      (emacsql db [:drop-table $i1] table)))
  (setq prog-snippet-db--connection nil))

(provide 'prog-snippet-db)
;; snippet-db.el ends here
