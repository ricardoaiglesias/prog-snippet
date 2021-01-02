# Prog Snippets
An environment used to store your programming notes. 

## The Problem
Often, I find myself looking up how to use a particular library or what the
idiomatic version of a piece of code is.

Normally, I would need to keep either a LaTeX or other such document with all
the code snippets and explanations, but this runs into some problems: 

1. Lookup is difficult. 
   - Using raw text to organize the code snippets works to some extent, but it
     is difficult to search without more meaningful representations of the data.
     For example, if you were to use "grep", it doesn't differentiate between
     "How to use a specific algorithm with vectors" and "Here's a piece of code
     that uses vectors". The use of tags would help this. 
2. Organization is difficult
   - Hierarchical filesystems quickly run into problems once you want to search
     through files. Do you place something in "C++/Vectors/" or "/Vectors/C++/"?
     And what do you do if a snippet belongs in several categories?
   - This further motivates the use of tags for lookup.
     
## Solution

This is a work in progress, but so far, here are some of the design decisions
     taken to solve this problem: 
* Use a SQLite Database (via emacs-sqlite) that maps a file to a set of tags.
* Use org-mode to allow for a rich organization to the content within a file.
* Allow for several different lookup methods
  * Currently supported lookup methods include the use of a "Primary Tag", as
    well as a "Union" set (where a file is included in the search results if it
    contains at least one of the tags in the set) and an "Intersection" set (a
    file is included in the search results if the file contains all of the tags
    in the intersection set)

    
## Current Progress
Currenty, there's a minimum viable product for the goals set forth by the above
document. You can create files, search them, add tags, modify titles, and so on. 

As of now, here are the things that are implemented: 

| Action                              | Function                                         |
|-------------------------------------|--------------------------------------------------|
| File Creation                       | `prog-snippet--create-file`                      |
| Modify Title                        | `prog-snippet--modify-title`                     |
| List Current File's tags            | `prog-snippet--get-file-tags`                    |
| Query Tags (Simple)                 | `prog-snippet--basic-search`                     |
| Query Tags (More Advanced)          | `prog-snippet--full-search`                      |
| Query and Paste all Matches in File | `prog-snippet--export-query-results`             |
| Init / Delete Database              | `prog-snippet--initialize / prog-snippet--reset` |


Note that you need to run `prog-snippet--initialize` to create and load the
database. Running `prog-snippet--reset` WILL delete all entries in the database
(but won't modify the files), and so is not recommended to run unless there's
something really wrong. 


