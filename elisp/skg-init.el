;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Load everything the skg client needs.
;;;
;;; USER-FACING FUNCTIONS AND GLOBAL VARIABLES
;;;   elisp/skg-state.el
;;;     skg-id-stack (variable)
;;;   elisp/skg-client.el
;;;     skg-disconnect
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-heralds-view
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-branch-aliases
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-branch-containerward
;;;     skg-branch-sourceward
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-save
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-view-from-node-id
;;;   elisp/skg-request-verify-connection.el
;;;     skg-verify-connection
;;;   elisp/skg-request-git-diff-mode.el
;;;     skg-diff-view
;;;   elisp/skg-id-search.el
;;;     skg-visit-link
;;;     skg-next-id
;;;     skg-previous-id
;;;     skg-push-to-linkstack
;;;     skg-validate-id-stack-buffer
;;;     skg-replace-id-stack-from-buffer
;;;     skg-linkstack-view
;;;   elisp/skg-new-empty-view.el
;;;     skg-new-empty-view
;;;   elisp/skg-buffer.el
;;;     skg-open-empty-content-view
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-metadata-view
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-search-titles
;;;   elisp/skg-user-facing-aliases.el
;;;     skg-set-indefinitive

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
