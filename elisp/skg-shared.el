;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read the data files in 'shared/', which the server's tests
;;; and both clients read: 'herald-styles.json' (the herald styles and
;;; how relationship heralds use them) and 'relations.json' (the
;;; node-node relations). The Neovim analog is nvim/lua/skg/shared.lua.

(require 'cl-lib)
(require 'json)

(defconst skg-shared-directory
  (expand-file-name
   "../shared/"
   (file-name-directory (or load-file-name buffer-file-name)))
  "The 'shared/' directory, found relative to this file.")

(defun skg-shared--read-json (file-name)
  "Parse FILE-NAME in `skg-shared-directory'. Objects become alists
with symbol keys, and arrays become lists."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file-name skg-shared-directory))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil)))

(defconst skg-shared-herald-styles
  (skg-shared--read-json "herald-styles.json")
  "The parsed 'shared/herald-styles.json'.")

(defconst skg-shared-relations
  (alist-get 'relations (skg-shared--read-json "relations.json"))
  "The relations in 'shared/relations.json', in the file's order.")

(defun skg-shared-styles ()
  "The herald styles: an alist from style name (a symbol) to its look."
  (alist-get 'styles skg-shared-herald-styles))

(defun skg-shared-style-keywords ()
  "Each style name in capitals, as the served rule table spells it."
  (mapcar (lambda (style) (intern (upcase (symbol-name (car style)))))
          (skg-shared-styles)))

(defun skg-shared-relations-in-display-order ()
  "The relations, sorted by their display position."
  (sort (copy-sequence skg-shared-relations)
        (lambda (a b) (< (alist-get 'display_position a)
                         (alist-get 'display_position b)))))

(defun skg-shared-relation (name)
  "The relation named NAME (a string), or nil."
  (cl-find name skg-shared-relations
           :key (lambda (relation) (alist-get 'name relation))
           :test #'equal))

(provide 'skg-shared)
