;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Load everything the skg client needs.
;;;
;;; USER-FACING FUNCTIONS AND GLOBAL VARIABLES
;;;   elisp/skg-state.el
;;;     skg-id-stack (variable)
;;;   elisp/skg-client.el
;;;     skg-doc-disconnect
;;;   elisp/heralds-minor-mode.el
;;;     heralds-minor-mode
;;;   elisp/skg-request-views.el
;;;     skg-request-aliases-view
;;;     skg-request-containerward-view
;;;     skg-request-sourceward-view
;;;   elisp/skg-request-save.el
;;;     skg-request-save-buffer
;;;   elisp/skg-request-single-root-content-view.el
;;;     skg-request-single-root-content-view-from-id
;;;   elisp/skg-request-verify-connection.el
;;;     skg-verify-connection
;;;   elisp/skg-id-search.el
;;;     skg-visit-link
;;;     skg-next-id
;;;     skg-previous-id
;;;     skg-push-id-to-stack
;;;     skg-validate-id-stack-buffer
;;;     skg-replace-id-stack-from-buffer
;;;     skg-edit-id-stack
;;;   elisp/skg-buffer.el
;;;     skg-open-empty-content-view
;;;   elisp/skg-request-title-matches.el
;;;     skg-request-title-matches

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
   (expand-file-name "elisp/skg-sexpr" project-root)) )

(require 'skg-client)
(require 'skg-id-search)
(require 'skg-buffer)
(require 'skg-request-views)
(require 'skg-request-title-matches)
(require 'skg-sexpr)
