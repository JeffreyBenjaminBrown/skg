;;; -*- lexical-binding: t; -*-
;;
;; PURPOSE: Load everything the skg client needs.

;; Add project directories to load-path
(let ((project-root
       (file-name-directory
        (directory-file-name
         (file-name-directory
          (or load-file-name buffer-file-name)) )) ))
  (add-to-list
   'load-path
   (expand-file-name "elisp" project-root))
  (add-to-list
   'load-path
   (expand-file-name "elisp/skg-sexpr" project-root)) )

(require 'skg-client)
(require 'skg-id-search)
(require 'skg-buffer)
(require 'skg-request-backward-view)
(require 'skg-request-title-matches)
(require 'skg-sexpr)
