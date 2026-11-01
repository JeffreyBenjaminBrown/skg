;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Handle the message the server sends unprompted when it
;;; has integrated '.skg' files that changed on disk (another writer,
;;; a 'git pull'): "repo-files-integrated", with (warnings (...)) and
;;; (view-ids (...)).
;;;
;;; A message whose warnings only count the files integrated per
;;; skgrepo is an echo-area message ("skg: repo first: 2 files
;;; integrated"). One that carries any other warning (a file that
;;; could not be read, a batch that failed validation, a leak) goes,
;;; whole, to `skg-server-warnings-buffer-name' under a headline "Repo
;;; changes", as the startup warnings do, and shows that buffer. The
;;; views it
;;; names show a changed node; each one whose buffer holds no unsaved
;;; edits is rerendered, through a "rerender all views" request that
;;; excludes every other view. A buffer with unsaved edits keeps them;
;;; the server refuses its save if it would write a node changed on
;;; disk since it was rendered.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Repo files
;;; integrated".

(require 'cl-lib)
(require 'skg-request-rerender-all-views)
(require 'skg-request-save)            ; for skg--message-list-nonempty-p
(require 'skg-request-server-warnings) ; for skg--append-to-server-warnings
(require 'skg-lock-buffers)            ; for skg--stream-in-progress
(require 'skg-state)

(defconst skg--repo-files-integrated-retry-seconds 0.5
  "How long to wait before asking again for a rerender that another
streaming request in flight blocked.")

(defconst skg--repo-files-integrated-retry-limit 120
  "How many times to ask again before giving up on a blocked rerender.")

(defconst skg--repo-files-integrated-count-regexp
  "\\`Integrated \\([0-9]+\\) files? that changed on disk in skgrepo '\\(.*\\)'\\.\\'"
  "A warning of a repo-files-integrated message that only counts the
files integrated in one skgrepo, as 'warnings_of_notice' in
'server/serve/handlers/repo_files_integrated.rs' words it. Group 1 is
the count, group 2 the skgrepo.")

(defun skg--repo-files-integrated-handler (_tcp-proc payload)
  "Show the warnings of a repo-files-integrated PAYLOAD and rerender
the views it names that have no unsaved edits."
  (condition-case err
      (let* ((response (read payload))
             (warnings (cadr (assoc 'warnings response)))
             (view-ids (mapcar (lambda (view-id) (format "%s" view-id))
                               (cadr (assoc 'view-ids response)))))
        (when (skg--message-list-nonempty-p warnings)
          (skg--show-repo-files-integrated-warnings warnings))
        (when view-ids
          ;; Not from inside the process filter: the rerender sends a request.
          (run-at-time 0 nil #'skg--rerender-views-changed-on-disk view-ids 0)))
    (error
     (message "skg: repo-files-integrated handler error: %S" err))))

(defun skg--show-repo-files-integrated-warnings (warnings)
  "Echo WARNINGS when each only counts the files integrated in a
skgrepo; otherwise append them all to the warnings buffer and show it."
  (if (cl-every (lambda (warning)
                  (string-match-p skg--repo-files-integrated-count-regexp
                                  warning))
                warnings)
      (message "%s" (skg--repo-files-integrated-echo-text warnings))
    (skg--append-to-server-warnings
     (concat "* Repo changes\n"
             (mapconcat (lambda (warning)
                          (concat "** " warning "\n"))
                        warnings ""))
     (format "skg: files changed on disk were integrated - see %s"
             skg-server-warnings-buffer-name))))

(defun skg--repo-files-integrated-echo-text (count-warnings)
  "\"skg: repo NAME: N files integrated\", one clause per warning of
COUNT-WARNINGS, each matching `skg--repo-files-integrated-count-regexp'."
  (concat "skg: "
          (mapconcat
           (lambda (warning)
             (string-match skg--repo-files-integrated-count-regexp warning)
             (let ((count (match-string 1 warning)))
               (format "repo %s: %s %s integrated"
                       (match-string 2 warning) count
                       (if (equal count "1") "file" "files"))))
           count-warnings "; ")))

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
      (if (< attempt skg--repo-files-integrated-retry-limit)
          (run-at-time skg--repo-files-integrated-retry-seconds nil
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

(setf (alist-get 'repo-files-integrated skg-unprompted-response-handler-map)
      #'skg--repo-files-integrated-handler)

(provide 'skg-repo-files-integrated)
