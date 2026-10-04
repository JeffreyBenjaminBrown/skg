;;; -*- lexical-binding: t; -*-
;;; Interactive, additive Markdown/Org import.

(require 'skg-config)
(require 'cl-lib)
(require 'skg-length-prefix)
(require 'skg-request-rerender-all-views)
(require 'skg-request-save)
(require 'skg-state)

(defconst skg--import-md-and-org-response-types
  '(import-md-and-org-host-mapping-needed
    import-md-and-org-preview import-md-and-org-result))

(defun skg-import-md-and-org (input-directory destination-source)
  "Preview importing INPUT-DIRECTORY into an owned DESTINATION-SOURCE.
Paths refer to the server filesystem.  The chosen source determines
privacy for every imported node.  No input file is changed."
  (interactive
   (list (read-string "Input directory on server (absolute path): ")
         (progn
           (message "Choose an owned source; it determines privacy for every imported node.")
           (skg--prompt-for-owned-source))))
  (unless destination-source (user-error "No owned source selected"))
  (skg--import-md-and-org-send
   `((action . "preview")
     (input-directory . ,input-directory)
     (destination-source . ,destination-source))
   input-directory destination-source))

(defun skg--import-md-and-org-cleanup ()
  (dolist (type skg--import-md-and-org-response-types)
    (setq skg-response-handler-map
          (assoc-delete-all type skg-response-handler-map))))

(defun skg--import-md-and-org-send (fields input-directory destination-source)
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
                (destination-source . ,destination-source)
                (host-root . ,host-root))
              input-directory destination-source))
         (quit (skg--import-md-and-org-cancel input-directory destination-source))))
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
                    input-directory destination-source)
                 (skg--import-md-and-org-cancel
                  input-directory destination-source))
             (quit (skg--import-md-and-org-cancel
                    input-directory destination-source))))))
     nil)
    (skg-register-response-handler
     'import-md-and-org-result
     (lambda (_tcp payload)
       (skg--import-md-and-org-cleanup)
       (let* ((response (read payload))
              (content (format "%s" (cadr (assoc 'content response))))
              (record-id (cadr (assoc 'record-id response))))
         (skg-big-nonfatal-message
         "*skg import result*" "Import result" content)
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

(defun skg--import-md-and-org-cancel (input-directory destination-source)
  (skg--import-md-and-org-send
   '((action . "cancel")) input-directory destination-source))

(defun skg--import-rerender-clean-views (&optional approved-pids)
  "Refresh clean views after import without replacing dirty client text."
  (let* ((dirty-uris
          (cl-loop for buf in (buffer-list)
                   for uri = (with-current-buffer buf
                               (and (boundp 'skg-view-uri) skg-view-uri))
                   when (and uri (buffer-modified-p buf))
                   collect uri))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "import rerender")
    (skg--lock-all-skg-buffers)
    (skg--register-rerender-stream-handlers)
    (skg--register-rerender-overPrivateText-confirmation
     (lambda (pids) (skg--import-rerender-clean-views pids)))
    (skg-lp-reset)
    (condition-case err
        (process-send-string
         tcp-proc
         (concat
          (prin1-to-string
           (append
            `((request . "rerender all views")
              (exclude-view-uris ,@dirty-uris))
            (when approved-pids
              `((allow-overPrivateText-telescopes ,@approved-pids)))))
          "\n"))
      (error
       (dolist (type '(rerender-lock rerender-view rerender-done
                       overPrivateText-telescope-confirmation))
         (let ((entry (assoc type skg-response-handler-map)))
           (when entry
             (when (cddr entry)
               (setq skg-lp--pending-count
                     (max 0 (1- skg-lp--pending-count))))
             (setq skg-response-handler-map
                   (assoc-delete-all type skg-response-handler-map)))))
       (skg--end-stream)
       (skg--unlock-all-save-locked)
       (signal (car err) (cdr err))))))

(provide 'skg-request-import-md-and-org)
