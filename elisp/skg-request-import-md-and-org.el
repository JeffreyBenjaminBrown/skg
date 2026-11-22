;;; skg-request-import-md-and-org.el --- Import Markdown and Org files  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; M-x 'skg-import-md-and-org' (no key) imports a directory of
;;; Markdown and Org files into an owned skgrepo, adding nodes and
;;; changing no input file. It asks for the input directory (a path on
;;; the server's filesystem) and the destination skgrepo
;;; ('skg--prompt-for-owned-repo'), which sets the privacy of every
;;; imported node; shows the server's preview in *skg import preview*,
;;; asking for a host root first when the server needs one; and on
;;; approval sends the preview's approval token, then shows the result
;;; in *skg import result* and, when the result names an import record
;;; ('record-id'), rerenders the views without unsaved edits
;;; ('skg--import-rerender-clean-views'). Declining sends a cancel
;;; ('skg--import-md-and-org-cancel'). Its Neovim twin is
;;; 'nvim/lua/skg/import_md_and_org.lua'.
;;;
;;; DATA USED/ASSUMED: See 'docs/api-and-formats.org', "Import Markdown
;;; and Org".
;;;
;;; Code:

(require 'skg-config)
(require 'skg-picker)
(require 'cl-lib)
(require 'skg-length-prefix)
(require 'skg-request-rerender-all-views)
(require 'skg-messages)
(require 'skg-state)

(defconst skg--import-md-and-org-response-types
  '(import-md-and-org-host-mapping-needed
    import-md-and-org-preview import-md-and-org-result))

(defun skg-import-md-and-org (input-directory destination-skgrepo)
  "Preview importing INPUT-DIRECTORY into an owned DESTINATION-REPO.
Paths refer to the server filesystem.  The chosen repo determines
privacy for every imported node.  No input file is changed."
  (interactive
   (list (read-string "Input directory on server (absolute path): ")
         (progn
           (message "Choose an owned repo; it determines privacy for every imported node.")
           (skg--prompt-for-owned-repo "Import into repo: "))))
  (unless destination-skgrepo (user-error "No owned repo selected"))
  (skg--import-md-and-org-send
   `((action . "preview")
     (input-directory . ,input-directory)
     (destination-repo . ,destination-skgrepo))
   input-directory destination-skgrepo))

(defun skg--import-md-and-org-cleanup ()
  (dolist (type skg--import-md-and-org-response-types)
    (setq skg-response-handler-map
          (assoc-delete-all type skg-response-handler-map))))

(defun skg--import-md-and-org-send (fields input-directory destination-skgrepo)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--import-md-and-org-cleanup)
    (skg-register-response-handler
     'import-md-and-org-host-mapping-needed
     (lambda (_tcp payload)
       (skg--import-md-and-org-cleanup)
       (skg-big-nonfatal-message
        "*skg import host mapping*" "Import needs a host root"
        (format "%s" (cadr (assoc 'content (read payload)))))
       (condition-case nil
           (let ((host-root
                  (read-string
                   "Absolute host path corresponding to input directory (blank leaves links unresolved): ")))
             (run-at-time
              0 nil #'skg--import-md-and-org-send
              `((action . "preview")
                (input-directory . ,input-directory)
                (destination-repo . ,destination-skgrepo)
                (host-root . ,host-root))
              input-directory destination-skgrepo))
         (quit (skg--import-md-and-org-cancel input-directory destination-skgrepo))))
     nil)
    (skg-register-response-handler
     'import-md-and-org-preview
     (lambda (_tcp payload)
       (skg--import-md-and-org-cleanup)
       (let* ((response (read payload))
              (content (format "%s" (cadr (assoc 'content response))))
              (token (cadr (assoc 'approval-token response))))
         (skg-big-nonfatal-message
          "*skg import preview*" "Import preview" content)
         (when token
           (condition-case nil
               (if (yes-or-no-p "Import exactly this preview? ")
                   (run-at-time
                    0 nil #'skg--import-md-and-org-send
                    `((action . "apply") (approval-token . ,(format "%s" token)))
                    input-directory destination-skgrepo)
                 (skg--import-md-and-org-cancel
                  input-directory destination-skgrepo))
             (quit (skg--import-md-and-org-cancel
                    input-directory destination-skgrepo))))))
     nil)
    (skg-register-response-handler
     'import-md-and-org-result
     (lambda (_tcp payload)
       (skg--import-md-and-org-cleanup)
       (let* ((response (read payload))
              (content (format "%s" (cadr (assoc 'content response))))
              (record-id (cadr (assoc 'record-id response))))
         (with-current-buffer
             (skg-big-nonfatal-message
              "*skg import result*" "Import result" content)
           (skg-report-mode 1)) ; so the record link can be followed
         (when record-id
           (run-at-time 0 nil #'skg--import-rerender-clean-views))))
     nil)
    (skg-lp-reset)
    (condition-case err
        (process-send-string
         tcp-proc
         (concat (prin1-to-string
                  (cons '(request . "import md and org") fields)) "\n"))
      (error (skg--import-md-and-org-cleanup)
             (signal (car err) (cdr err))))))

(defun skg--import-md-and-org-cancel (input-directory destination-skgrepo)
  (skg--import-md-and-org-send
   '((action . "cancel")) input-directory destination-skgrepo))

(defun skg--import-rerender-clean-views (&optional approved-pids)
  "Refresh clean views after import without replacing dirty client text."
  (let* ((dirty-view-ids
          (cl-loop for buf in (buffer-list)
                   for view-id = (with-current-buffer buf
                               (and (boundp 'skg-view-id) skg-view-id))
                   when (and view-id (buffer-modified-p buf))
                   collect view-id)))
    (skg--request-rerender-views-excluding
     "import rerender" dirty-view-ids approved-pids)))

(provide 'skg-request-import-md-and-org)
