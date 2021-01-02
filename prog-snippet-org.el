;;; prog-snippet-org.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author: Ricardo Iglesias <riglesia@stanford.edu>
;; URL
;; Keywords: convenience, prog-snippet, programming, reference
;; Package-Requires ((emacs "26.1") (emacsql "3.0.0")  (emacsql-sqlite3 "1.0.2"))
;;
;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This library provides an interface between the prog-snippet functions and the
;; files written (which will be org-mode files).

;;; Code:

;;; TODO: Add another link type that specifically queries for primary tags.

(require 'prog-snippet-vars)
(require 'prog-snippet-file)
(require 'prog-snippet-search)

(defvar prog-snippet-org--keymap nil
  "Keymap that calls prog-snippet functions." )

(defconst prog-snippet-org--link-prefix "prog-snippet"
  "Text that is placed before a link to specify that it is a tag
  that, when clicked, will load query results for that tag.")

(defun prog-snippet-org--initialize-keymap ()
  (when (null prog-snippet-org--keymap)
    (setq prog-snippet-org--keymap (make-sparse-keymap))
    (dolist (pair '(("c" . 'prog-snippet--create-file)
                    ("s" . 'prog-snippet--basic-search)
                    ("f" . 'prog-snippet--full-search)
                    ("t" . 'prog-snippet--get-file-tags)
                    ("m" . 'prog-snippet--modify-title)))
      (pcase pair
        (`(,key . ,fn)
         (define-key prog-snippet-org--keymap (kbd key) fn))))))

(defun prog-snippet-org--initialize-link-type ()
  (org-link-set-parameters prog-snippet-org--link-prefix
                           :follow #'prog-snippet-org--follow-link))

(defun prog-snippet-org--follow-link (tag _)
  (prog-snippet--basic-search tag))

(defun create-code-block (language)
  (format "#+BEGIN_SRC %s

#+END_SRC" language))

(defvar prog-snippet--primary-tag-to-string
  (list
   (cons "c"  (create-code-block "C"))
   (cons "c++"  (create-code-block "C++"))
   (cons "clojure" (create-code-block "clojure"))
   (cons "elisp"  (create-code-block "emacs-lisp"))
   (cons "java"  (create-code-block "java"))
   (cons "rust"  (create-code-block "rust"))
   (cons "sql"  (create-code-block "sql"))))

(defun prog-snippet-org--primary-to-string (primary)
  (let ((lookup (assoc primary prog-snippet--primary-tag-to-string)))
    (if lookup (cdr lookup) primary)))

(defun prog-snippet-org--link-follow (path)
  (insert (format "Followed path! %s" path)))

(defun prog-snippet-org--insert-tag (tag)
  (insert (format "[[prog-snippet:%s][%s]]" tag tag)))

(defun prog-snippet-org--create-preamble (file-info)
  (let ((id (oref file-info snippet-id))
        (tags (oref file-info tag-list))
        (primary-str (prog-snippet-org--primary-to-string
                      (oref file-info primary-tag)))
        (title (oref file-info title)))
    (format "#+TITLE: %s
#+FILE-ID: %s

* %s
** Code

%s

** Explanation

" title id title primary-str)))

(defun prog-snippet-org--create-new-file (file-info)
  (let ((path (slot-value file-info 'path)))
    (write-region (prog-snippet-org--create-preamble file-info) nil path)
    (find-file path)))

(defun prog-snippet-org--get-id (filepath)
  "Given `filepath', this function returns the ID for the
  snippet, which is written at the top"
  (save-excursion
    (find-file filepath)
    (goto-char (point-min))
    (let* ((id-point (search-forward "#+FILE-ID: "))
           (id-string
            (buffer-substring id-point (progn (goto-char id-point)
                                              (forward-line 1)
                                              (point)))))
      ;; Remove id's formatting, if any. 
      (set-text-properties 0
                           (length id-string)
                           nil
                           id-string)
      (string-trim id-string))))

(defun prog-snippet-org--get-id-in-current-file ()
  (interactive)
  (message "%s" (prog-snippet-org--get-id (buffer-file-name))))

(defun replace-all-in-file (from to)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward from nil t)
        (replace-match to nil t))))

(defun prog-snippet-org--modify-title (new-path new-title)
  (rename-file (buffer-file-name) new-path t)
  (find-file new-path)
  (save-excursion
    (goto-char (point-min))
    ;;  Replace all instances of the previous title with the new one.
    (let* ((title-pt (search-forward "#+TITLE: "))
           (title (string-trim
                   (buffer-substring title-pt (progn (goto-char title-pt)
                                                     (forward-line)
                                                     (point))))))
      (replace-all-in-file title new-title)))
  (save-buffer))

(provide 'prog-snippet-org)
