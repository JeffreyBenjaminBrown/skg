;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Load everything the skg client needs.
;;;
;;; USER-FACING FUNCTIONS AND GLOBAL VARIABLES
;;;   elisp/skg-state.el
;;;     skg-id-stack (variable)
;;;   elisp/skg-client.el
;;;     skg-connection-end
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-view-heralds-mode
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-local-aliases
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-local-containerward
;;;     skg-local-sourceward
;;;     skg-local-link-sourceward
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-save
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-view-from-node-id
;;;   elisp/skg-request-verify-connection.el
;;;     skg-connection-verify
;;;   elisp/skg-request-git-diff-mode.el
;;;     skg-view-diff-mode
;;;   elisp/skg-id-search.el
;;;     skg-visit-link
;;;     skg-id-next
;;;     skg-id-prev
;;;     skg-id-push
;;;     skg-validate-id-stack-buffer
;;;     skg-replace-id-stack-from-buffer
;;;     skg-id-stack
;;;   elisp/skg-view-new-empty.el
;;;     skg-view-new-empty
;;;   elisp/skg-buffer.el
;;;     skg-open-empty-content-view
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-view-metadata
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-search-titles
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-local-indefinitive

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
   (expand-file-name "elisp/skg-request-save" project-root)) )

(require 'skg-log)
(require 'skg-buffer)
(require 'skg-client)
(require 'skg-id-search)
(require 'skg-request-title-matches)
(require 'skg-request-views)
(require 'skg-sexpr-edit)
(require 'skg-sexpr-search)
(require 'skg-view-org-ancestry)
