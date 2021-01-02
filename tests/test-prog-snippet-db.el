;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'emacsql-sqlite3)
(require 'emacsql)
(require 'dash)
(require 'seq)
(require 'subr-x)

(require 'cl)
(require 'prog-snippet-db)

(defun prog-snippet-test--create-db-test-db ()
  "Creates the test database for the 'test-prog-snippet-db' tests."
  (let* ((temp-name (format "%s.sqlite" (make-temp-name "prog-snippet-test-")))
         (db (prog-snippet-db--get-temp-db temp-name)))
    (prog-snippet-db--create-tables db)
    (prog-snippet-db--batch-insert
     db
     '("File A" "C++" ("vector" "map" "stl" "iterator"))
     '("File B" "Elisp" ("macro" "map" "quote"))
     '("File C" "Elisp" ("macro" "threading" "dash"))
     '("File D" "Rust" ("tuple" "structs"))
     '("File E" "Rust" ("ownership" "borrow" "mutable"))
     '("File F" "Python" ("range" "list"))
     '("File G" "Python" ("numpy" "data-analysis"))
     '("File H" "Elisp" ("vector" "map" "iterator" "macro" "lists"))
     '("File I" "C++" ("map" "iterator" "const"))
     )
    (list db temp-name)))

(describe "Database Queries"
  :var (db name)

  (before-all
    (let ((result (prog-snippet-test--create-db-test-db)))
      (insert (format "%s" result))
      (setq db (first result))
      (setq name (second result))))

  (before-each
    (unless db (user-error "DB is null.")))

  (after-all
    (prog-snippet-db--delete-db db name))

  ;; Union functionality. Results must have at least one of the tags.
  (it "handles unions with one tag"
    (unless db (user-error "Database is Null... Initialization of database failed."))
    (expect (length (prog-snippet-db--search db '("map") '()))
            :to-equal 4))
  (it "handles unions with one tag, with primary tag."
    (expect (length (prog-snippet-db--search db '("map") '() "c++"))
            :to-equal 2))
  (it "handles unions with multiple tags"
    (expect (length (prog-snippet-db--search
                     db '("map" "macro" "numpy") '()))
            :to-equal 6))
  (it "handles unions with multiple tags, with primary tag."
    (expect (length (prog-snippet-db--search
                     db '("map" "macro" "numpy") '() "c++"))
            :to-equal 2))
  (it "handles unions with no tags."
    (expect (length (prog-snippet-db--search
                     db '() '()))
            :to-equal 9))

  ;; Intersection functionality. Results must have ALL the tags
  (it "handles intersection with one tag"
    (expect (length (prog-snippet-db--search db '() '("map")))
            :to-equal 4))
  (it "handles intersections with one tag, with primary tag."
    (expect (length (prog-snippet-db--search db '() '("map") "c++"))
            :to-equal 2))
  (it "handles intersections with multiple tags"
    (expect (length (prog-snippet-db--search
                     db '() '("map" "macro" "quote")))
            :to-equal 1))
  (it "handles intersections with multiple tags, with primary tag."
    (expect (length (prog-snippet-db--search
                     db '() '("map" "macro" "quote") "c++"))
            :to-equal 0)
    (expect (length (prog-snippet-db--search
                     db '() '("map" "macro" "quote") "elisp"))
            :to-equal 1))

  ;; Complete query functionality (Union + Intersection)
  (it "handles queries with multiple tags"
    (expect (length (prog-snippet-db--search
                     db '("threading" "stl" "quote") '("map")))
            :to-equal 4))
  (it "handles queries with multiple tags, with primary tag."
    (expect (length (prog-snippet-db--search
                     db '("map" "macro" "quote") '("vector" "iterator") "c++"))
            :to-equal 1)
    (expect (length (prog-snippet-db--search
                     db '("map" "macro" "quote") '("vector" "iterator" "lists") "elisp"))
            :to-equal 1))

  ;; General-tags
  (it "correctly gathers unique tags"
    (expect (length (prog-snippet-db--get-tags db)) :to-equal 23)
    (expect (length (prog-snippet-db--get-tags db "c++")) :to-equal 6)))
