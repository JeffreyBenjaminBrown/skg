;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Load everything the skg client needs.

(let ;; Add project directories to load-path
    ((project-root
      (file-name-directory
       (directory-file-name
        (file-name-directory
         (or load-file-name buffer-file-name)) )) ))
  (add-to-list
   'load-path
   (expand-file-name "elisp" project-root))
  (add-to-list
   'load-path
   (expand-file-name "elisp/skg-sexpr" project-root))
  (add-to-list
   'load-path
   (expand-file-name "elisp/skg-request-save" project-root))
  (add-to-list
   'load-path
   (expand-file-name "elisp/skg-sexpr-edit" project-root)) )

(require 'skg-log)
(require 'skg-buffer)
(require 'skg-client)
(require 'skg-id-search)
(require 'skg-request-text-search)
(require 'skg-request-views)
(require 'skg-sexpr-edit)
(require 'skg-sexpr-search)
(require 'skg-view-org-ancestry)
(require 'skg-readable-ids)
(require 'skg-file-minor-mode)
(require 'skg-git-add)
