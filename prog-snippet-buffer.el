;;; prog-snippet-buffer.el -*- coding: utf-8; lexical-binding: t; -*-
;; Author: Ricardo Iglesias <riglesia@stanford.edu>
;; URL
;; Keywords: convenience, prog-snippet, programming, reference
;; Package-Requires ((emacs "26.1") (emacsql "3.0.0")  (emacsql-sqlite3 "1.0.2"))
;;
;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This library is in charge of "drawing" the query results. AS of now, this is
;; just creating an org-mode buffer and writing some text... But hey, it works.

(require 'widget)
(require 'prog-snippet-search)

(defgroup prog-snippet-faces nil
  "Faces used in prog-snippet."
  :group 'prog-snippet :group 'faces)

(defface snippet-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Snippet header"
  :group 'prog-snippet-faces)

(defface snippet-subheading-face
  '((t :inherint font-lock-keyword-face :italic t))
  ""
  :group 'prog-snippet-faces)

(defface snippet-filter-string-face
  '((t :inherit font-lock-string-face))
  "Faces for search terms."
  :group 'prog-snippet-faces)

(defface snippet-tag-face
  '((t :inherit font-lock-type-face))
  ""
  :group 'prog-snippet-faces)

(defconst snippet-search-buffer-name
  "*snippet-search*"
  "Snippet Buffer name.")

(defface prog-snippet-link-face
  '((t :inherit font-lock-keyword-face))
  ""
  :group 'prog-snippet-faces)

(defun pad-right (str len)
  (concat str (make-string (- len (length str)) ?\s)))

(defun input-string-to-tags (input-str tags)
  (let ((words (split-string input-str)))
    (seq-filter (lambda (word)
                  (find (downcase word) tags :test 'equal ))
                words)))

(defun prog-snippet-buffer--create-query-link (tag)
  (format "[[prog-snippet:%s][%s]]" tag tag))

(defun prog-snippet-buffer--draw-query-results (query-input query-results)
  (switch-to-buffer "*prog-snippet:query-results*")
  (erase-buffer)

  (insert (format "#+query-input: %s\n\n" query-input))

  (unless (equal major-mode 'org-mode) (org-mode))
  (dolist (row query-results)
    (pcase row
      (`(,id ,tags ,title ,path ,primary)
       (insert (format "* [[file:%s][%s]] (%s)\n" path title
                       (prog-snippet-buffer--create-query-link primary)))
       (--> (format " - ID: %s\n" id)
            (propertize it 'font-lock-face '(:foreground "#555555"))
            (insert it))
       (insert " - Tags : ")
       (insert (format "%s" (mapconcat 'prog-snippet-buffer--create-query-link
                                       (cl-sort tags 'string-lessp :key 'downcase)
                                       ", " )))
       (insert "\n\n")))))

(provide 'prog-snippet-buffer)
;; prog-snippet-buffer.el ends here
