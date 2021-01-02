;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'emacsql-sqlite3)
(require 'dash)
(require 'seq)

(require 'cl)

(require 'prog-snippet-db)
(require 'prog-snippet-search)

(defun generate-alist (primary-str union-str intersect-str)
  (list (cons :primary primary-str)
        (cons :union union-str)
        (cons :intersect intersect-str)))

(defun prog-snippet--create-test-db-search ()
  (let* ((temp-name (concat (make-temp-name "prog-snippet-test-") ".sqlite"))
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
     '("File H" "Elisp" ("vector" "map" "stl" "iterator"))
     '("File I" "Elisp" ("tag"))
     '("File J" "C++" ("map" "iterator" "const")))
    (list db temp-name)))

(defun assoc-equal? (lhs rhs)
  (mapcar (lambda (alist-elem)
            (pcase alist-elem
              (`(,key . ,value) (equal value (cdr (assoc key rhs))))))
          lhs))

(describe "Interface to database"
  :var (db name)

  (before-all
    (let ((result (prog-snippet--create-test-db-search)))
      (setq db (first result))
      (setq name (second result))))
  
  (before-each
    (when (null db) (error "Database Uninitialized. Please check the initialization code."))
    (spy-on 'prog-snippet-db--search :and-call-through))

  (after-all
    (prog-snippet-db--delete-db db name))


  (it "Raises an error on empty input"
    (expect (prog-snippet-search--query (generate-alist "" "" "") db)
            :to-throw 'user-error))

  (it "Performs a Union search when the intersect string is empty"
    (prog-snippet-search--query (generate-alist "" "tag" "") db)
    (prog-snippet-search--query (generate-alist "Elisp" "tag" "") db)
    (expect 'prog-snippet-db--search :to-have-been-called-times 2) )


  (it "Performs an Intersect search when the union string is empty."
    (prog-snippet-search--query (generate-alist "" "" "tag") db)
    (prog-snippet-search--query (generate-alist "Elisp" "" "tag") db)
    (expect 'prog-snippet-db--search :to-have-been-called-times 2))

  (it "Performs a joint Union-Intersect search when neither string is empty."
    ;;  Without primary tag.
    (prog-snippet-search--query (generate-alist "" "tag" "tag") db)
    ;;  With primary tag
    (prog-snippet-search--query (generate-alist "Elisp" "tag" "tag") db)
    (expect 'prog-snippet-db--search :to-have-been-called-times 2)))

(describe "Conversion from Strings to Tags"
  :var (db name tags)
  (before-all
    (let ((result (prog-snippet--create-test-db-search)))
      (setq db (first result))
      (setq name (second result))
      (setq tags (prog-snippet-db--get-tags db))))


  (before-each
    (when (null db) (error "Database Uninitialized. Please check the initialization code.")))

  (after-all
    (prog-snippet-db--delete-db db name))

  (it "returns empty lists when given empty strings"
    (expect (prog-snippet-search--sanitize-string "" tags) :to-equal '()))

  (it "ignores tags that are not in the database"
    (expect (prog-snippet-search--sanitize-string "not-in-db" tags)
            :to-equal '()))

  (it "removes duplicate valid tags."
    (expect (prog-snippet-search--sanitize-string "elisp elisp elisp" tags)
            :to-equal '("elisp")))

  (it "accepts tags inside the database."
    (expect (prog-snippet-search--sanitize-string "elisp" tags)
            :to-equal '("elisp")))

  (it "performs a case-insensitive transformation."
    (expect (prog-snippet-search--sanitize-string "ELISP" tags)
            :to-equal '("elisp"))))
