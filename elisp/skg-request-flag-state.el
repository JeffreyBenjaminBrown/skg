;;; -*- lexical-binding: t; -*-
;;;
;;; Show and change the user-mutable NoSearchMatching flag.

(require 'cl-lib)
(require 'org)
(require 'skg-config)
(require 'skg-length-prefix)
(require 'skg-metadata)

(defconst skg--search-matching-choices
  '("search matching" "no search matching"))

(defun skg-set-flag-search-matching (&optional recursive)
  "Stage whether the node at point may match text searches.
With prefix argument RECURSIVE, apply the selected desired state to the
true-content subtree.  This modifies metadata but does not save."
  (interactive "P")
  (unless (org-at-heading-p) (user-error "Not on a headline"))
  (let* ((metadata (skg--metadata-sexp-at-point-or-nil))
         (skgid (skg--flag-eligible-root-id metadata))
         (buffer (current-buffer))
         (marker (point-marker)))
    (skg-register-response-handler
     'flag-state
     (lambda (_tcp-proc payload)
       (skg--set-flag-search-matching-from-state
        buffer marker skgid recursive payload))
     t)
    (skg-lp-reset)
    (process-send-string
     (skg-tcp-connect-to-rust)
     (concat
      (prin1-to-string
       `((request . "flag state")
         (id . ,skgid)
         (flag . "noSearchMatching")))
      "\n"))))

(defun skg--flag-eligible-root-id (metadata)
  "Validate root METADATA and return its saved graph ID."
  (unless (skg--unrestrictedNode-sexp-p metadata)
    (user-error "Search matching can be set only on an unrestricted node"))
  (when (skg--node-write-protected-p metadata)
    (user-error "Cannot set search matching on a write-protected node"))
  (when (skg-sexp-cdr-at-path metadata '(skg node editRequest))
    (user-error "This node already has an editRequest"))
  (or (skg--node-id metadata)
      (user-error "Save the node first; it has no graph ID")))

(defun skg--set-flag-search-matching-from-state
    (buffer marker expected-skgid recursive payload)
  "Handle a flag-state PAYLOAD, guarding BUFFER/MARKER against staleness."
  (let* ((response (read payload))
         (string-value (lambda (key)
                         (let ((entry (assoc key response)))
                           (and entry (format "%s" (cadr entry))))))
         (error-message (funcall string-value 'error))
         (canonical-skgid (funcall string-value 'id))
         (value (funcall string-value 'value))
         (owned (funcall string-value 'owned)))
    (run-at-time
     0 nil
     (lambda ()
       (unless (buffer-live-p buffer)
         (user-error "skg: buffer vanished before the search-matching prompt"))
       (with-current-buffer buffer
         (save-excursion
           (unless (and (marker-buffer marker)
                        (<= (point-min) marker) (<= marker (point-max)))
             (user-error "skg: headline vanished before the search-matching prompt"))
           (goto-char marker)
           (let* ((metadata (skg--metadata-sexp-at-point-or-nil))
                  (current-skgid (and metadata (skg--node-id metadata))))
             (when error-message
               (user-error "flag state: %s" error-message))
             (unless (and current-skgid
                          (or (string= current-skgid expected-skgid)
                              (and canonical-skgid
                                   (string= current-skgid canonical-skgid))))
               (user-error "The headline changed while flag state was loading"))
             (unless (string= owned "true")
               (user-error "Cannot set search matching on a foreign node"))
             ;; Re-run the local refusal after the network round trip.
             (skg--flag-eligible-root-id metadata)
             (let* ((initial (if (string= value "true")
                                 "no search matching"
                               "search matching"))
                    (choice (skg--completing-read-with-cycle
                             "Search behavior (S-left/right cycle): "
                             skg--search-matching-choices nil t initial
                             nil nil nil skg--search-matching-choices))
                    (desired (string= choice "no search matching")))
               (if recursive
                   (skg--stage-flag-search-matching-recursive desired)
                 (skg--stamp-search-matching-request desired)
                 (message "Search matching staged for 1 node. Save to apply."))))))))))

(defun skg--stamp-search-matching-request (no-search-matching)
  (skg-edit-metadata-at-point
   `(skg (node (editRequest
                (flag noSearchMatching
                          ,(if no-search-matching 'true 'false)))))))

(defun skg--stage-flag-search-matching-recursive (desired)
  "Stage DESIRED uniformly through the true-content subtree at point."
  (let ((targets (skg--flag-recursive-targets))
        (seen (make-hash-table :test #'equal))
        (changed 0)
        (skipped nil))
    (dolist (marker targets)
      (save-excursion
        (goto-char marker)
        (let* ((meta (skg--metadata-sexp-at-point-or-nil))
               (skgid (and meta (skg--node-id meta)))
               (reason
                (cond
                 ((not skgid) "no saved ID")
                 ((gethash skgid seen) "duplicate occurrence")
                 ((skg--node-write-protected-p meta) "write-protected")
                 ((skg-sexp-cdr-at-path meta '(skg node editRequest))
                  "already has an editRequest")
                 ((not (member (skg--node-repo meta)
                               (skg--owned-repos))) "foreign repo"))))
          (cond
           ((equal reason "duplicate occurrence") nil)
           (reason (push (format "%s (%s)" (or skgid "no-id") reason) skipped))
           (t (puthash skgid t seen)
              (skg--stamp-search-matching-request desired)
              (setq changed (1+ changed))))))
      (set-marker marker nil))
    (message "Search matching staged for %d node%s; skipped: %s. Save to apply."
             changed (if (= changed 1) "" "s")
             (if skipped (mapconcat #'identity (nreverse skipped) ", ") "none"))))

(defun skg--flag-recursive-targets ()
  "Return markers for root and unrestricted true-content descendants only."
  (save-excursion
    (org-back-to-heading t)
    (let ((targets (list (copy-marker (line-beginning-position))))
          (root-level (org-outline-level)))
      (outline-next-heading)
      (while (and (not (eobp)) (> (org-outline-level) root-level))
        (let ((meta (skg--metadata-sexp-at-point-or-nil)))
          (if (and (skg--unrestrictedNode-sexp-p meta)
                   (skg--node-affectsParent-content-of-p meta))
              (progn
                (push (copy-marker (line-beginning-position)) targets)
                (outline-next-heading))
            (skg--goto-next-headline-after-subtree))))
      (nreverse targets))))

(provide 'skg-request-flag-state)
