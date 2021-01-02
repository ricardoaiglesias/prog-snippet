;;; prog-snippet-vars.el -*- coding: utf-8; lexical-binding: t; -*-
;;;
;; Author: Ricardo Iglesias <riglesia@stanford.edu>
;; URL
;; Keywords: convenience, prog-snippet, programming, reference
;; Package-Requires ((emacs "26.1"))
;;
;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This file defines variables used throughout the prog-snippet library 
 

(defcustom prog-snippet-dir "~/.snippets/"
  "Directory where both new snippets are added and the snippet
  database is stored"
  :type 'string
  :group 'prog-snippet)

(defconst prog-snippet-tag-link-type "prog-tag"
  "Prefix denoting a link as belonging to a prog-snippet tag.")

(provide 'prog-snippet-vars)
