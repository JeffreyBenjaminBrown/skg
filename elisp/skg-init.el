;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Load everything the skg client needs.
;;;
;;; USER-FACING FUNCTIONS AND GLOBAL VARIABLES
;;;   elisp/skg-state.el
;;;     skg-id-stack (variable)
;;;   elisp/skg-client.el
;;;     skg-connection-end
;;;   elisp/skg-keymaps-and-aliases.el
;;;     skg-local-aliases
;;;     skg-local-containerward
;;;     skg-local-indefinitive
;;;     skg-local-link-sourceward
;;;     skg-local-metadata
;;;     skg-local-sourceward
;;;     skg-save
;;;     skg-search-titles
;;;     skg-view-from-node-id
;;;     skg-view-heralds-mode
;;;   elisp/skg-buffer.el
;;;     skg-open-empty-content-view
;;;   elisp/skg-id-search.el
;;;     skg-id-next
;;;     skg-id-prev
;;;     skg-id-push
;;;     skg-id-stack
;;;     skg-replace-id-stack-from-buffer
;;;     skg-validate-id-stack-buffer
;;;     skg-visit-link
;;;   elisp/skg-request-git-diff-mode.el
;;;     skg-view-diff-mode
;;;   elisp/skg-request-verify-connection.el
;;;     skg-connection-verify
;;;   elisp/skg-view-new-empty.el
;;;     skg-view-new-empty

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
(require 'skg-request-title-matches)
(require 'skg-request-views)
(require 'skg-sexpr-edit)
(require 'skg-sexpr-search)
(require 'skg-view-org-ancestry)
