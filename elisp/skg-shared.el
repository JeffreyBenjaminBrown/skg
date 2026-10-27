;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read the data files in 'shared/', which the server's tests
;;; and both clients read: 'herald-styles.json' (the herald styles and
;;; how relationship heralds use them), 'relations.json' (the
;;; node-node relations) and 'metadata-fields.json' (the fields a user
;;; may set on each kind of headline). The Neovim analog is
;;; nvim/lua/skg/shared.lua.

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

(defconst skg-shared-metadata-fields
  (skg-shared--read-json "metadata-fields.json")
  "The parsed 'shared/metadata-fields.json'.")

(defconst skg-shared-metadata-kinds
  (alist-get 'kinds skg-shared-metadata-fields)
  "The headline kinds in 'shared/metadata-fields.json', each with the
fields a user may set on it.")

(defconst skg-shared-metadata-parts
  (alist-get 'parts skg-shared-metadata-fields)
  "The parts of '(skg ...)' in the order the server writes them, from
'shared/metadata-fields.json': strings, \"KIND\" standing for a kind's
atom.")

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

(defun skg-shared-metadata-kind (atom)
  "The kind in 'shared/metadata-fields.json' known by the atom ATOM (a
string), or nil."
  (cl-find-if (lambda (kind) (member atom (alist-get 'atoms kind)))
              skg-shared-metadata-kinds))

(provide 'skg-shared)
