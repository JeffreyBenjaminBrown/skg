;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Handle the message the server sends unprompted when it
;;; has reloaded '.skg' files that changed on disk (another writer,
;;; a 'git pull'): "repo-files-reloaded", with (reloaded ((SKGREPO
;;; N) ...)), (warnings (...)) and (view-ids (...)).
;;;
;;; A message with no warnings is an echo-area message of its counts
;;; ("skg: reloaded 2 files from repo first"). One that carries a warning
;;; (a file that could not be read, a batch that failed validation, a
;;; leak) goes, counts and warnings, to `skg-server-warnings-buffer-name'
;;; under a headline "Repo changes", as the startup warnings do, and
;;; shows that buffer. The views it names show a changed node; each one whose buffer holds no unsaved
;;; edits is rerendered, through a "rerender all views" request that
;;; excludes every other view; the server also extends each buffer of
;;; the diff view among them to the changes it does not show. A buffer
;;; with unsaved edits keeps them; the server refuses its save if it
;;; would write a node changed on disk since it was rendered.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Repo files
;;; reloaded".

(require 'cl-lib)
(require 'skg-request-rerender-all-views)
(require 'skg-request-save)            ; for skg--message-list-nonempty-p
(require 'skg-request-server-warnings) ; for skg--append-to-server-warnings
(require 'skg-lock-buffers)            ; for skg--stream-in-progress
(require 'skg-state)

(defconst skg--repo-files-reloaded-retry-seconds 0.5
  "How long to wait before asking again for a rerender that another
streaming request in flight blocked.")

(defconst skg--repo-files-reloaded-retry-limit 120
  "How many times to ask again before giving up on a blocked rerender.")

(defun skg--repo-files-reloaded-handler (_tcp-proc payload)
  "Show the counts and warnings of a repo-files-reloaded PAYLOAD and
rerender the views it names that have no unsaved edits."
  (condition-case err
      (let* ((response (read payload))
             (count-clauses (skg--repo-files-reloaded-count-clauses
                             (cadr (assoc 'reloaded response))))
             (warnings (cadr (assoc 'warnings response)))
             (view-ids (mapcar (lambda (view-id) (format "%s" view-id))
                               (cadr (assoc 'view-ids response)))))
        (skg--show-repo-files-reloaded count-clauses warnings)
        (when view-ids
          ;; Not from inside the process filter: the rerender sends a request.
          (run-at-time 0 nil #'skg--rerender-views-changed-on-disk view-ids 0)))
    (error
     (message "skg: repo-files-reloaded handler error: %S" err))))

(defun skg--show-repo-files-reloaded (count-clauses warnings)
  "Echo COUNT-CLAUSES when there are no WARNINGS; otherwise append the
clauses and the warnings to the warnings buffer and show it."
  (cond
   ((skg--message-list-nonempty-p warnings)
    (skg--append-to-server-warnings
     (concat "* Repo changes\n"
             (mapconcat (lambda (line) (concat "** " line "\n"))
                        (append count-clauses warnings) ""))
     (format "skg: files changed on disk were reloaded - see %s"
             skg-server-warnings-buffer-name)))
   (count-clauses
    (message "skg: %s" (mapconcat #'identity count-clauses "; ")))))

(defun skg--repo-files-reloaded-count-clauses (reloaded)
  "\"reloaded N files from repo NAME\" for each (NAME N) of RELOADED."
  (mapcar (lambda (skgrepo-and-count)
            (let ((count (cadr skgrepo-and-count)))
              (format "reloaded %s %s from repo %s"
                      count (if (equal count 1) "file" "files")
                      (car skgrepo-and-count))))
          reloaded))

(defun skg--rerender-views-changed-on-disk (view-ids attempt)
  "Rerender each open buffer among VIEW-IDS that holds no unsaved
edits, excluding every other view. While another streaming request is
in flight, ask again later; ATTEMPT counts the tries."
  (let* ((open-buffers (skg--open-view-buffers))
         (to-rerender
          (cl-loop for (view-id . buffer) in open-buffers
                   when (and (member view-id view-ids)
                             (not (buffer-modified-p buffer)))
                   collect view-id))
         (excluded
          (cl-loop for (view-id . _buffer) in open-buffers
                   unless (member view-id to-rerender)
                   collect view-id)))
    (cond
     ((null to-rerender) nil)
     (skg--stream-in-progress
      (if (< attempt skg--repo-files-reloaded-retry-limit)
          (run-at-time skg--repo-files-reloaded-retry-seconds nil
                       #'skg--rerender-views-changed-on-disk
                       view-ids (1+ attempt))
        (message "skg: views showing files changed on disk were not rerendered; %s"
                 "rerender them with skg-request-rerender-all-views")))
     (t (skg--request-rerender-views-excluding
         "repo-change rerender" excluded)))))

(defun skg--open-view-buffers ()
  "Each live skg view buffer, as (VIEW-ID . BUFFER)."
  (cl-loop for buffer in (buffer-list)
           for view-id = (and (buffer-live-p buffer)
                              (buffer-local-value 'skg-view-id buffer))
           when view-id
           collect (cons view-id buffer)))

(setf (alist-get 'repo-files-reloaded skg-unprompted-response-handler-map)
      #'skg--repo-files-reloaded-handler)

(provide 'skg-repo-files-reloaded)
