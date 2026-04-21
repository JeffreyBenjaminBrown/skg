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
;;;     skg-make-definitive
;;;     skg-save
;;;     skg-search
;;;     skg-search-interactive
;;;     skg-show-aliases
;;;     skg-show-containerward
;;;     skg-show-link-sourceward
;;;     skg-show-sourceward
;;;     skg-view-heralds-mode
;;;     skg-view-metadata
;;;   elisp/skg-buffer.el
;;;     skg-open-empty-content-view
;;;   elisp/skg-id-search.el
;;;     skg-goto
;;;     skg-goto-by-id
;;;     skg-id-next
;;;     skg-id-prev
;;;     skg-id-push
;;;     skg-replace-id-stack-from-buffer
;;;     skg-validate-id-stack-buffer
;;;     skg-view-id-stack
;;;     skg-visit-link
;;;   elisp/skg-metadata.el
;;;     skg-delete
;;;     skg-delete-recursive
;;;     skg-make-indefinitive
;;;   elisp/skg-request-file-path.el
;;;     skg-goto-in-magit
;;;     skg-goto-in-magit-parent
;;;   elisp/skg-request-git-diff-mode.el
;;;     skg-view-diff-mode
;;;   elisp/skg-request-verify-connection.el
;;;     skg-connection-verify
;;;   elisp/skg-show-org-ancestry.el
;;;     skg-show-org-ancestry
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
(require 'skg-request-text-search)
(require 'skg-request-views)
(require 'skg-sexpr-edit)
(require 'skg-sexpr-search)
(require 'skg-show-org-ancestry)
(require 'skg-magit-titles)
(require 'skg-file-minor-mode)
