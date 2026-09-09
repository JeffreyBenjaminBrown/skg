;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-org-fold)
(require 'skg-focus)
(require 'skg-metadata)
(require 'skg-buffer)
(require 'skg-config)
(require 'skg-lock-buffers)
(require 'skg-pending-save)

(defvar-local skg--last-rendered-content nil
  "Last server rendering installed in this view.")

(defvar-local skg--disk-client-conflict nil
  "Structured metadata for an unresolved out-of-band disk conflict.")

(defvar-local skg--disk-conflict-resolution-in-progress nil
  "Non-nil while an explicitly reconciled conflict is being saved.")

(defun skg-mark-disk-client-conflict (conflict)
  "Install CONFLICT and make this view logically dirty until resolution."
  (setq skg--disk-client-conflict conflict)
  (when skg--buffer-record
    (setf (skg--buffer-record-logical-dirty skg--buffer-record) t)))

(defun skg-clear-disk-client-conflict ()
  "Terminate this view's Ediff bundle and clear its conflict state."
  (let ((reviews (alist-get 'review-buffers skg--disk-client-conflict))
        (control (alist-get 'ediff-control skg--disk-client-conflict)))
    (when (buffer-live-p control)
      (with-current-buffer control
        (when (fboundp 'ediff-quit)
          (let ((ediff-keep-variants t))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _) t)))
              (ediff-quit nil))))))
    (dolist (buffer reviews)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (set-buffer-modified-p nil))
        (kill-buffer buffer))))
  (setq skg--disk-client-conflict nil)
  (when skg--buffer-record
    (setf (skg--buffer-record-logical-dirty skg--buffer-record) nil)))

(defvar-local skg--application-token 0
  "Monotonic identity of the server text most recently applied here.")

(defvar-local skg--background-refresh-stale nil
  "Non-nil when a background refresh could not safely be applied.")

(defun skg--other-unsaved-skg-buffers ()
  "Return the list of skg view buffers OTHER than the current one that
have unsaved modifications (`buffer-modified-p')."
  (let ((self (current-buffer))
        (result nil))
    (dolist (buf (buffer-list))
      (when (and (not (eq buf self))
                 (buffer-local-value 'skg-view-uri buf)
                 (buffer-modified-p buf))
        (push buf result)))
    result))

(defun skg--confirm-save-despite-other-unsaved ()
  "plan_v2 §8.4: if other skg buffers have unsaved edits that this save's
collateral rerenders might overwrite, warn loudly and ask before sending.
Signals an error (aborting the save) if the user declines. Skipped in
batch mode (`noninteractive'), where there is no user to ask -- and the
over-warning is the accepted tradeoff (narrowing to the truly-affected set
would need the slow SavePlan we don't have yet)."
  (when (not noninteractive)
    (let ((others (skg--other-unsaved-skg-buffers)))
      (when others
        (unless (yes-or-no-p
                 (format
                  "DANGER: %d other skg buffer(s) have unsaved edits (%s) this save may overwrite. Save anyway? "
                  (length others)
                  (mapconcat #'buffer-name others ", ")))
          (error "Save aborted: other skg buffers have unsaved edits"))))))

(defun skg-request-save-buffer (&optional fork-approved fork-sources
                                          hoist-approved-pids
                                          scalar-approved-pids)
  "Send the current buffer contents to Rust for processing.
Before sending, adds 'folded' markers to folded headlines and 'focused' marker to current headline.
The server sends three LP messages around the save:
  1. save-lock: early, broad lock (every buffer sharing a pid).
  2. save-relax-lock: narrows the lock to the exact collateral set once
     the SavePlan is known, plus a collateral-view per rerendered buffer.
  3. save-result: the saved buffer's final content (+ errors/warnings).
All skg buffers are locked immediately; non-collateral buffers are
unlocked as save-lock / save-relax-lock / collateral-view arrive.

If the save edited any FOREIGN node, the server reads that as a request
to fork (clone) it and -- unless FORK-APPROVED is non-nil -- replies with
a `fork-confirmation' message instead of `save-result', committing
nothing. `skg--fork-confirmation-handler' then shows the confirmation
buffer and offers `skg-approve-fork' (re-save with FORK-APPROVED) /
`skg-decline-fork'."
  (interactive)
  (unless skg--buffer-record
    (user-error "Cannot save: this buffer has no explicit Skg application record"))
  (when-let ((reason (skg-known-save-restriction)))
    (user-error "Cannot save while %s; nothing was saved; try again when ready"
                reason))
  (skg-tcp-connect-to-rust)
  (unless (skg-connection-handshake-ensure)
    (user-error "Cannot save before server session verification completes"))
  (when-let ((reason (skg-known-save-restriction)))
    (user-error "Cannot save while %s; nothing was saved; try again when ready"
                reason))
  (when (and skg--disk-client-conflict
             (not skg--disk-conflict-resolution-in-progress))
    (user-error
     "Save blocked by a disk-client conflict; run M-x skg-resolve-disk-client-conflict"))
  (skg-pending-save-assert-none-unresolved)
  (skg--confirm-save-despite-other-unsaved)
  (when (org-before-first-heading-p)
    ;; Rather than complain, save as if point were at the first headline.
    (goto-char (point-min))
    (outline-next-heading))
  (let ((focused-had-metadata ;; Whether the focused headline already has metadata. Storing this lets us clean up the bare (skg) that removal leaves behind.
         (save-excursion
           (org-back-to-heading t)
           (looking-at "\\*+ (skg")))
        (save-point-position
         (skg--current-save-point-position)))
    (skg-add-folded-markers)
    (skg-add-focused-marker)
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (save-buffer (current-buffer))
           (saved-uri skg-view-uri)
           (save-authority skg--buffer-record)
           (buffer-contents (buffer-string))
           (operation-id (skg-pending-save-new-operation-id))
           (request-intent
            (prin1-to-string
             (skg--save-request-sexp
              skg-view-uri save-point-position fork-approved fork-sources
              hoist-approved-pids scalar-approved-pids save-authority
              operation-id)))
           (request-base-fingerprint
            (skg-pending-save-fingerprint request-intent buffer-contents))
           (request-s-exp (concat (prin1-to-string
                                   (skg--save-request-sexp
                                    skg-view-uri
                                    save-point-position
                                    fork-approved
                                    fork-sources
                                    hoist-approved-pids
                                    scalar-approved-pids
                                    save-authority operation-id
                                    request-base-fingerprint))
                                  "\n"))
           pending-record)
      (progn ;; Rust needs these markers, but the user doesn't.
        (skg-remove-focused-marker)
        (skg-remove-folded-markers))
      (unless focused-had-metadata
        (skg-strip-bare-skg-at-focused-headline))

      (unless skg-view-uri
        ;; Guard: refuse to save when skg-view-uri is nil.
        ;; A nil view-uri causes an unfiltered save (all instructions
        ;; sent to TypeDB even if unchanged) AND the server won't update
        ;; its in-Rust graph, so the work is both slow and wasted.
        (error "Cannot save: skg-view-uri is nil in buffer '%s' (content-view-mode=%s). Re-open the view."
               (buffer-name)
               (if (derived-mode-p 'skg-content-view-mode) "on" "off")))

      (when skg--stream-in-progress
        (error "skg: save blocked -- %s already in progress"
               skg--stream-in-progress))
      (setq pending-record
            (skg-pending-save-prepare
             :operation-id operation-id
             :request-base-fingerprint request-base-fingerprint
             :request request-s-exp
             :content buffer-contents
             :buffer-id (skg--buffer-record-id save-authority)))

      (skg--begin-stream "save")
      (skg--register-stream-request-cleanup "save")
      (skg-set-request-failure-handler
       (lambda (reason)
         (condition-case err
             (skg-pending-save-mark-uncertain pending-record)
           (error
            (skg-log 'error 'save
                     "could not retain uncertain save %s: %S"
                     operation-id err)))
         (message "skg: save interrupted: %s; operation %s retained"
                  reason operation-id)))

      ;; Lock ALL skg content-view buffers immediately, before sending.
      ;; This eliminates the race window between the send and the
      ;; server's early response.
      (skg--lock-all-skg-buffers)

      ;; Register handlers in this request's dispatch record.
      (skg-register-response-handler
       'save-lock
       (lambda (_tcp-proc payload)
         (skg--save-lock-handler saved-uri payload))
       t)
      ;; save-relax-lock: same shape/handling as save-lock, but with the
      ;; EXACT collateral view set (post-SavePlan), so buffers locked early that
      ;; aren't actually collateral get unlocked. The saved buffer stays
      ;; locked (skg--unlock-non-collateral-buffers keeps saved-uri) until
      ;; save-result. Registered NON-one-shot (like collateral-view) so it does
      ;; NOT add to skg-lp--pending-count: an *invalid* save errors before the
      ;; server reaches the point that emits save-relax-lock, so a one-shot
      ;; count would leak (never decremented) and hang the next save's wait.
      ;; save-result removes it.
      (skg-register-response-handler
       'save-relax-lock
       (lambda (_tcp-proc payload)
         (skg--save-lock-handler saved-uri payload))
       nil)
      (skg-register-response-handler
       'collateral-view
       (lambda (_tcp-proc payload)
         (skg--collateral-view-handler payload))
       nil) ;; non-one-shot: fires for each streamed collateral view
      (skg-register-response-handler
       'save-result
       (lambda (response-proc payload)
         (when (eq (skg--persist-save-response pending-record payload)
                   'committed)
           (skg--save-result-handler save-buffer payload)
           (skg--schedule-save-result-acknowledgement
            response-proc pending-record)))
       t)
      ;; fork-confirmation: the ALTERNATIVE terminal message to
      ;; save-result. The server sends exactly one of the two. Registered
      ;; NON-one-shot (does NOT bump skg-lp--pending-count); whichever
      ;; terminal handler fires removes the other (and the fork handler
      ;; decrements the count for the unfired save-result one-shot), so
      ;; the pending count balances either way.
      (skg-register-response-handler
       'fork-confirmation
       (lambda (_tcp-proc payload)
         (when (eq (skg--persist-save-response pending-record payload)
                   'refused)
           (skg--fork-confirmation-handler save-buffer payload)))
       nil)
      ;; The other alternative terminal. It carries no scalar text; after
      ;; approval, retry this same save with the exact listed PIDs.
      (skg-register-response-handler
       'telescope-hoist-confirmation
       (lambda (_tcp-proc payload)
         (when (eq (skg--persist-save-response pending-record payload)
                   'refused)
           (skg--telescope-hoist-confirmation-handler
            save-buffer payload fork-approved fork-sources
            scalar-approved-pids)))
       nil)
      (skg-register-response-handler
       'ugly-telescope-confirmation
       (lambda (response-proc payload)
         (pcase (skg--persist-save-response pending-record payload)
           ('committed
            (skg--save-scalar-release-confirmation-handler
             save-buffer payload fork-approved fork-sources
             hoist-approved-pids)
            (skg--schedule-save-result-acknowledgement
             response-proc pending-record))))
       nil)
      (skg-register-response-handler
       'error
       (lambda (_tcp-proc payload)
         (let ((response (car (read-from-string payload))))
           (skg--persist-save-response pending-record payload)
           (ding)
           (message "SKG request failed: %s"
                    (or (cadr (assoc 'content response)) payload))))
       nil)

      (skg-submit-request tcp-proc request-s-exp buffer-contents))))

(defun skg--save-request-sexp (view-uri save-point-position
                                        &optional fork-approved fork-sources
                                        hoist-approved-pids
                                        scalar-approved-pids authority
                                        operation-id request-base-fingerprint)
  "Build the save-buffer request sexp. When FORK-APPROVED is non-nil,
include (fork-approved . \"true\") so the server commits any forks it
finds instead of returning a fork-confirmation. FORK-SOURCES, when
non-nil, is an alist ((N . SOURCE) ...) pairing each forked node's id
with the owned source the user chose for its clone; it rides out as the
field (fork-sources ((N . SOURCE) ...))."
  (append
   `((request . "save buffer")
     (view-uri . ,view-uri)
     (point-lines-below-focused-headline
      . ,(number-to-string
          (plist-get save-point-position
                     :point-lines-below-focused-headline)))
     (point-column
      . ,(number-to-string
          (plist-get save-point-position
                     :point-column)))
     (point-screen-lines-below-window-start
      . ,(number-to-string
          (plist-get save-point-position
                     :point-screen-lines-below-window-start))))
   (when fork-approved
     '((fork-approved . "true")))
   (when fork-sources
     (list (list 'fork-sources fork-sources)))
   (when hoist-approved-pids
     `((hoist-approved-pids ,@hoist-approved-pids)))
   (when scalar-approved-pids
     `((allow-ugly-telescopes ,@scalar-approved-pids)))
   (when operation-id
     `((operation-id . ,operation-id)))
   (when authority
     `((client-buffer-id . ,(skg--buffer-record-id authority))
       (view-kind . ,(symbol-name (skg--buffer-record-kind authority)))
       (graph-generation
        . ,(number-to-string
            (or (skg--buffer-record-graph-generation authority) 0)))
       (server-revision
        . ,(number-to-string
            (or (skg--buffer-record-server-revision authority) 0)))
       (client-application-token
        . ,(number-to-string
            (or (skg--buffer-record-application-token authority) 0)))
       (server-session-id
        . ,(or (skg--buffer-record-server-session-id authority)
               (error "Skg save authority has no server session")))))
   ;; This must remain the final pair: the server reconstructs the exact
   ;; canonical intent by removing it before hashing intent + NUL + body.
   (when request-base-fingerprint
     `((request-base-fingerprint . ,request-base-fingerprint)))))

(defun skg--persist-save-response (record payload)
  "Persist PAYLOAD according to its explicit ordinary-save outcome."
  (let* ((response (car (read-from-string payload)))
         (state (cadr (assq 'save-operation-state response))))
    (skg-pending-save-verify-response record response)
    (pcase state
      ('committed
       (skg-pending-save-mark-terminal record payload)
       'committed)
      ('refused
       (skg-pending-save-mark-terminal record payload t)
       'refused)
      ('blocked
       (skg-pending-save-mark-uncertain record)
       (message "Save operation %s is blocked: %s"
                (skg-pending-save--field record 'operation-id)
                (or (cadr (assq 'reason response)) "status required"))
       'blocked)
      (_ (skg-pending-save--fail
          "save response lacks a valid save-operation-state")))))

(defun skg--schedule-save-result-acknowledgement (tcp-proc record)
  "Acknowledge RECORD after its terminal response has been handled locally."
  (run-at-time
   0 nil
   (lambda ()
     (condition-case err
         (let ((operation-id
                (skg-pending-save--field record 'operation-id))
               (fingerprint
                (skg-pending-save--field
                 record 'request-base-fingerprint)))
           (skg-register-response-handler
            'save-operation-ack
            (lambda (_ack-proc payload)
              (let ((response (car (read-from-string payload))))
                (skg-pending-save-verify-response record response)
                (skg-pending-save-mark-acknowledged record)))
            t)
           (skg-set-request-failure-handler (lambda (_reason) nil))
           (skg-submit-request
            (if (process-live-p tcp-proc)
                tcp-proc
              (skg-tcp-connect-to-rust))
            (concat
             (prin1-to-string
              `((request . "acknowledge save result")
                (operation-id . ,operation-id)
                (request-base-fingerprint . ,fingerprint)))
             "\n")))
       (error
        (skg-log 'error 'save
                 "save result acknowledgement retained for recovery: %S"
                 err))))))

(defun skg--choose-pending-save ()
  (let ((records (skg-pending-save-unresolved-records)))
    (unless records (user-error "There is no unresolved save"))
    (if (= (length records) 1)
        (car records)
      (let* ((ids (mapcar (lambda (record)
                            (skg-pending-save--field record 'operation-id))
                          records))
             (chosen (completing-read "Pending save: " ids nil t)))
        (cl-find chosen records :test #'equal
                 :key (lambda (record)
                        (skg-pending-save--field record 'operation-id)))))))

(defun skg-pending-save-inspect ()
  "Open the private exact record for one unresolved ordinary save."
  (interactive)
  (let* ((record (skg--choose-pending-save))
         (path (skg-pending-save--path
                (skg-pending-save--root)
                (skg-pending-save--field record 'operation-id))))
    (find-file-read-only path)))

(defun skg-pending-save-status ()
  "Ask the server for one unresolved ordinary save's durable status."
  (interactive)
  (let* ((record (skg--choose-pending-save))
         (operation-id (skg-pending-save--field record 'operation-id))
         (fingerprint
          (skg-pending-save--field record 'request-base-fingerprint))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'save-operation-status
     (lambda (_proc payload)
       (let* ((response (car (read-from-string payload)))
              (updated
               (skg-pending-save-apply-status record response))
              (server-state
               (skg-pending-save--field updated 'server-state)))
         (message
          (if (eq (skg-pending-save--field updated 'fresh-view-required)
                  'true)
              "Save operation %s is %s; reopen a fresh view, then inspect and acknowledge it"
            "Save operation %s is %s; use M-x skg-pending-save-inspect")
          operation-id server-state)))
     t)
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "save operation status")
         (operation-id . ,operation-id)
         (request-base-fingerprint . ,fingerprint)))
      "\n"))))

(defun skg-acknowledge-pending-save ()
  "Acknowledge an inspected terminal result and compact its local record."
  (interactive)
  (let* ((record (skg--choose-pending-save))
         (operation-id (skg-pending-save--field record 'operation-id))
         (fingerprint
          (skg-pending-save--field record 'request-base-fingerprint))
         (state (skg-pending-save--field record 'state))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (unless (eq state 'terminal)
      (user-error "Save %s has no inspected terminal result" operation-id))
    (skg-register-response-handler
     'save-operation-ack
     (lambda (_proc payload)
       (let ((response (car (read-from-string payload))))
         (skg-pending-save-verify-response record response)
         (skg-pending-save-mark-acknowledged record)
         (message "Save operation %s acknowledged" operation-id)))
     t)
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "acknowledge save result")
         (operation-id . ,operation-id)
         (request-base-fingerprint . ,fingerprint)))
      "\n"))))

(defun skg-retry-pending-save ()
  "Explicitly retry exact bytes for an unknown/prepared save identity."
  (interactive)
  (let* ((record (skg--choose-pending-save))
         (material (skg-pending-save-retry-material record))
         (operation-id (skg-pending-save--field record 'operation-id))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "pending-save retry")
    (skg--register-stream-request-cleanup "pending-save retry")
    (skg-set-request-failure-handler
     (lambda (reason)
       (skg-pending-save-mark-uncertain record)
       (message "Save retry interrupted: %s; operation %s retained"
                reason operation-id)))
    (skg-register-response-handler 'save-lock (lambda (&rest _) nil) t)
    (dolist (frame-kind
             '(save-relax-lock collateral-view fork-confirmation
               telescope-hoist-confirmation ugly-telescope-confirmation
               error))
      (skg-register-response-handler
       frame-kind
       (lambda (_proc payload)
         (skg--persist-save-response record payload)
         (message "Save operation %s returned; inspect the retained outcome"
                  operation-id))
       nil))
    (skg-register-response-handler
     'save-result
     (lambda (_proc payload)
       (skg--persist-save-response record payload)
       (message "Save operation %s returned; inspect the retained outcome"
                operation-id))
     t)
    (skg-submit-request tcp-proc (car material) (cadr material))))

(defun skg--current-save-point-position ()
  "WHAT IT DOES: Return point position data that should survive the save redraw:
- text-line offset: relative to the focused headline
- column: the character offset of point within its line
- screen-line offset: relative to the top *visible* line of the window
WHY: A character offset from buffer start is not stable enough: saving can make branches appear or disappear above point. The focused headline is already preserved by metadata, so record point relative to that headline and to the window top. The within-line column is stable regardless of what happens above, so it is recorded as a plain offset."
  (let ((point-line (line-number-at-pos (point) t))
        (window (get-buffer-window (current-buffer) t)))
    (list :point-lines-below-focused-headline
          (save-excursion
            (org-back-to-heading t)
            (- point-line (line-number-at-pos (point) t)))
          :point-column
          (- (point) (line-beginning-position))
          :point-screen-lines-below-window-start
          (if window
              (max 0
                   (count-screen-lines
                    (window-start window)
                    (point)
                    nil
                    window))
            0))))

(defun skg-strip-bare-skg-at-focused-headline ()
  "Remove bare (skg) from the current headline if that is its only metadata.
Used after marker removal to clean up headlines that had no metadata
before the add/remove cycle."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at "\\(\\*+ \\)(skg) ")
      (replace-match "\\1"))))

(defun skg--save-lock-handler (saved-uri payload)
  "Handle the save-lock LP message (tagged with response-type).
Unlocks non-collateral buffers."
  (condition-case err
      (let* ((response (read payload))
             (lock-entry (assoc 'lock-views response)))
        (when lock-entry
          (let ((collateral-uris (cadr lock-entry)))
            (skg--unlock-non-collateral-buffers
             saved-uri collateral-uris))))
    (error
     ;; Keep the saved buffer locked until save-result (unlocking everything
     ;; here would let the user edit it during the rest of the pipeline, and the
     ;; subsequent erase+insert would silently drop those edits); free the rest.
     (skg--unlock-non-collateral-buffers saved-uri nil)
     (skg-log 'error 'save "save-lock handler error: %S" err)) ))

(defun skg--apply-streamed-view-update (payload log-category handler-name)
  "Apply one streamed view update from PAYLOAD: unlock and replace the buffer for
its view URI.  Retained only for the legacy in-request save stream;
LOG-CATEGORY and HANDLER-NAME label any error."
  (condition-case err
      (let* ((response (read payload))
             (uri (cadr (assoc 'view-uri response)))
             (content (cadr (assoc 'content response)))
             (buf (skg-find-buffer-by-uri uri)))
        (when buf
          (with-current-buffer buf
            (let ((session
                   (skg-require-current-server-session
                    response skg--buffer-record)))
              (skg--unlock-after-save)
              (if (buffer-modified-p)
                  (progn
                    (skg-mark-disk-client-conflict
                     `((reason . "stream arrived after local modification")
                       (incoming . ,content)))
                    (ding)
                    (skg-log 'error log-category
                             "%s refused to overwrite newly dirty buffer %s"
                             handler-name (buffer-name)))
                (skg-replace-buffer-with-new-content
                 nil content nil (list :server-session-id session)))))))
    (error (skg-log 'error log-category
                    "%s handler error: %S" handler-name err))))

(defun skg--collateral-view-handler (payload)
  "Handle one streamed collateral-view update.
Unlocks and updates the buffer for the given view URI."
  (skg--apply-streamed-view-update payload 'save "collateral-view"))

(defun skg--save-result-handler (save-buffer payload)
  "Handle the full save-result LP message (tagged with response-type).
Removes the collateral-view handler, unlocks all save-locked buffers,
then processes the save response.
Unlock must happen BEFORE `skg-handle-save-sexp' because
`skg-replace-buffer-with-new-content' calls erase-buffer + insert,
which would trigger overlay modification-hooks if still present."
  (dolist (frame-kind
           '(collateral-view save-relax-lock fork-confirmation
             telescope-hoist-confirmation ugly-telescope-confirmation))
    (skg-remove-response-handler frame-kind))
  (skg--end-stream)
  (unwind-protect
      (progn
        (skg--unlock-all-save-locked)
        (with-current-buffer save-buffer
          (skg-handle-save-sexp payload)))
    (skg--unlock-all-save-locked)) )

(defconst skg-fork-source-placeholder "PICK-A-SOURCE"
  "Sentinel source the server pre-fills for a clone-to-be whose source
the user has not specified (in the saved metadata or a prior round).
`skg--fork-choose-placeholder-sources' prompts for a replacement per
carrying clone (the user can also set one with C-c s s);
`skg-approve-fork' refuses while any remains. Must match
FORK_SOURCE_PLACEHOLDER in server/from_text/fork.rs.")

(defvar-local skg--fork-origin-buffer nil
  "In a fork-confirmation buffer, the source buffer whose save raised the
forks. `skg-approve-fork' re-saves it (approved); `skg-decline-fork'
leaves things untouched.")

(defvar-local skg--fork-suppress-strip-on-kill nil
  "When non-nil, `skg--fork-confirmation-on-kill' does NOT strip the
origin's fork atom. `skg-approve-fork' sets it before killing this
buffer, because its re-save still needs the atom to commit the fork.")

(defun skg--fork-confirmation-on-kill ()
  "`kill-buffer-hook' for a fork-confirmation buffer: dismissing it
WITHOUT approving strips the lingering (viewRequests fork) atom from the
origin buffer, so the next save does not silently re-fork. This covers
killing the buffer directly (C-x k, q, etc.); `skg-decline-fork' already
strips explicitly, and `skg-approve-fork' suppresses this (its re-save
needs the atom, and the server drops it on re-render). Stripping is
idempotent, so a redundant call after a decline is a harmless no-op."
  (unless skg--fork-suppress-strip-on-kill
    (when (buffer-live-p skg--fork-origin-buffer)
      (with-current-buffer skg--fork-origin-buffer
        (skg-strip-fork-requests-in-buffer)))))

(defun skg--fork-confirmation-handler (save-buffer payload)
  "Handle a `fork-confirmation' LP message: the save edited foreign
node(s) and was not pre-approved, so NOTHING was committed. Show the
confirmation buffer (which lists the nodes that would be forked) and,
interactively, prompt for any clone source not yet specified, then ask
whether to approve.

Terminal, like `skg--save-result-handler': remove the streaming handlers
AND the unfired save-result one-shot (decrementing the pending count for
it), end the stream, and unlock."
  (dolist (frame-kind
           '(collateral-view save-relax-lock fork-confirmation
             telescope-hoist-confirmation ugly-telescope-confirmation
             save-result))
    (skg-remove-response-handler frame-kind))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (let ((confirm-buf
         ;; The condition-case guards only response parsing/display.
         ;; The interactive flow below runs OUTSIDE it: a refusal
         ;; (user-error) from `skg-approve-fork' must reach the user,
         ;; not the log -- nesting it here used to swallow the
         ;; \"pick a source first\" refusal, so approving with a
         ;; placeholder source silently did nothing.
         (condition-case err
             (let* ((response (read payload))
                    (content (cadr (assoc 'content response)))
                    (to-minibuffer (cadr (assoc 'to-minibuffer response)))
                    (buf (skg--show-fork-confirmation content save-buffer)))
               (when to-minibuffer (message "%s" to-minibuffer))
               buf)
           (error
            (skg-log 'error 'save "fork-confirmation handler error: %S" err)
            nil))))
    (when (and confirm-buf (not noninteractive))
      ;; In batch (tests) the caller drives skg-approve-fork /
      ;; skg-decline-fork directly; interactively, ask now. Quitting
      ;; (C-g) any prompt leaves the confirmation buffer open: set
      ;; sources with C-c s s and approve with C-c C-c, or decline
      ;; with C-c C-k.
      (with-current-buffer confirm-buf
        (skg--fork-choose-placeholder-sources)
        (if (yes-or-no-p "Fork the listed node(s)? ")
            (skg-approve-fork)
          (skg-decline-fork))))))

(defun skg--telescope-hoist-confirmation-handler
    (save-buffer payload fork-approved fork-sources
                 &optional scalar-approved-pids)
  "Handle the text-free terminal Hoist challenge for SAVE-BUFFER.
The server has committed nothing.  On approval, reissue the same save with
the exact candidate PIDs; on Abort, leave the buffer and every .skg file
untouched.  FORK-APPROVED and FORK-SOURCES survive if this challenge arose
on a retry that had already received fork authority."
  (dolist (frame-kind
           '(collateral-view save-relax-lock fork-confirmation
             telescope-hoist-confirmation ugly-telescope-confirmation
             save-result))
    (skg-remove-response-handler frame-kind))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (condition-case err
      (let* ((response (read payload))
             (telescopes (cadr (assoc 'telescopes response)))
             (approved-pids
              (mapcar (lambda (entry)
                        (format "%s" (cadr (assoc 'pid entry))))
                      telescopes))
             (prompt (or (cadr (assoc 'prompt response))
                         "Hoist lower title/body text to home? ")))
        (if noninteractive
            (message "Hoist required for %s; nothing was saved"
                     (mapconcat #'identity approved-pids ", "))
          (if (yes-or-no-p prompt)
              (with-current-buffer save-buffer
                (skg-request-save-buffer
                 fork-approved fork-sources approved-pids
                 scalar-approved-pids))
            (message
             "Hoist aborted; nothing was saved. Repair the .skg sections manually."))))
    (error
     (skg-log 'error 'save
              "telescope-hoist-confirmation handler error: %S" err))))

(defun skg--save-scalar-release-confirmation-handler
    (save-buffer payload fork-approved fork-sources hoist-approved-pids)
  "Handle a save-rerender scalar release challenge.
The filesystem save has succeeded, but the server has not released the
staged saved/collateral text or changed its open-view registry.  Approval
reissues the save with the exact PIDs; declining leaves the current buffers
unchanged."
  (dolist (response-type
           '(collateral-view save-relax-lock fork-confirmation
             telescope-hoist-confirmation ugly-telescope-confirmation
             save-result))
    (skg-remove-response-handler response-type))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (condition-case err
      (let* ((response (read payload))
             (approved-pids
              (mapcar (lambda (pid) (format "%s" pid))
                      (cadr (assoc 'pids response))))
             (prompt (or (cadr (assoc 'prompt response))
                         "Display staged text from ugly telescopes? ")))
        (if noninteractive
            (message "Saved, but protected rerender text was withheld for %s"
                     (mapconcat #'identity approved-pids ", "))
          (if (yes-or-no-p prompt)
              (with-current-buffer save-buffer
                (skg-request-save-buffer
                 fork-approved fork-sources hoist-approved-pids
                 approved-pids))
            (message
             "Save succeeded; protected rerender text remains withheld and buffers are unchanged."))))
    (error
     (skg-log 'error 'save
              "save scalar-release confirmation handler error: %S" err))))

(defun skg--fork-suggested-source-above-point ()
  "Return the suggested source named by the comment directly above the
headline at point, or nil. The server writes that comment above each
clone-to-be whose source the user has not yet specified."
  (save-excursion
    (forward-line -1)
    (when (looking-at
           "^# Suggested source for the clone below: \\(.+\\)$")
      (string-trim (match-string 1)))))

(defun skg--fork-choose-placeholder-sources ()
  "Prompt for an owned source for each clone-to-be still carrying
`skg-fork-source-placeholder', writing the choice into the buffer.
The server's suggested source (the comment above the clone) is the
default; S-left/S-right cycle through the sources you own. A no-op
when every clone's source is already specified -- notably when the
saved metadata itself specified it (the server then omits the
placeholder), per TODO/fork-fixes.org: no redundant ask."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (beginning-of-line)
      (let ((sexp (skg--metadata-sexp-at-point-or-nil)))
        (when (and (= (org-current-level) 1)
                   sexp
                   (equal (skg--node-source sexp)
                          skg-fork-source-placeholder))
          (let* ((title (nth 2 (skg-split-as-stars-metadata-title
                                (skg-get-current-headline-text))))
                 (suggested (skg--fork-suggested-source-above-point))
                 (owned (skg--owned-sources))
                 (default (or suggested (car owned)))
                 (choice (skg--completing-read-with-cycle
                          (format "Source for the clone \"%s\" (default %s): "
                                  title default)
                          owned nil t nil nil default nil owned)))
            (skg--change-source-at-point
             (if (string-empty-p choice) default choice)))))
      (forward-line 1))))

(defun skg--show-fork-confirmation (content save-buffer)
  "Show CONTENT in the *SKG Fork Confirmation* buffer, recording
SAVE-BUFFER as its origin, and return the buffer. The buffer is a
navigable content view (so id-push / search work). It is editable so
each clone's source can be set (the handler's minibuffer prompts write
into it; C-c s s on a clone-to-be works too), but it is NOT an ordinary
save target: skg-view-uri is left nil (tripping the nil-view-uri save
guard) and C-x C-s is rebound to refuse, because a stray normal save of
its id-less clone-to-be parents would create bare nodes. Only C-c C-c
\(approve) and C-c C-k (decline) act on it."
  (let ((buf (skg-acquire-generated-buffer "*SKG Fork Confirmation*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content ""))
        (skg-content-view-mode)
        (goto-char (point-min)))
      ;; nil view-uri: this is a registered attached workflow, not a live
      ;; graph view, and the ordinary-save guard rejects saving it directly.
      (setq skg-view-uri nil)
      (setq skg--fork-origin-buffer save-buffer)
      (setq skg--fork-suppress-strip-on-kill nil)
      ;; Dismissing this buffer without approving (killing it directly,
      ;; not via C-c C-k) must still strip the origin's fork atom.
      (add-hook 'kill-buffer-hook #'skg--fork-confirmation-on-kill nil t)
      ;; Copy the mode map first so these overrides stay buffer-local --
      ;; local-set-key mutates (current-local-map) in place, which is the
      ;; shared skg-content-view-mode-map; rebinding C-x C-s on the shared
      ;; map would break saving in every content view.
      (use-local-map (copy-keymap (current-local-map)))
      ;; C-c C-c commits the forks; C-c C-k declines; C-x C-s refuses
      ;; (this buffer must not be saved as ordinary content).
      (local-set-key (kbd "C-c C-c") #'skg-approve-fork)
      (local-set-key (kbd "C-c C-k") #'skg-decline-fork)
      (local-set-key (kbd "C-x C-s") #'skg--fork-confirmation-refuse-save)
      (set-buffer-modified-p nil)
      (skg-register-buffer
       buf 'fork-confirmation
       :lifecycle 'attached-workflow
       :disposable nil
       :continuation-id (org-id-uuid)
       :origin-buffer save-buffer
       :origin-location "((scope save))"
       :recipe '((kind . "fork-confirmation"))
       :last-fetched (skg-buffer-raw-text buf)))
      (when (fboundp 'heralds-minor-mode) (heralds-minor-mode))
    (display-buffer buf)
    buf))

(defun skg--fork-confirmation-refuse-save ()
  "Refuse an ordinary save of the fork-confirmation buffer.
Its clone-to-be parents are id-less owned-source nodes; saving them as
ordinary content would create bare nodes. Approve with C-c C-c (commits
the forks) or decline with C-c C-k."
  (interactive)
  (user-error
   "This is the fork-confirmation buffer; use C-c C-c to approve or C-c C-k to decline"))

(defun skg--fork-sources-from-confirmation-buffer ()
  "Walk the fork-confirmation buffer; return an alist ((N . SOURCE) ...).
Each level-1 headline is a clone-to-be carrying (source SOURCE) and no
id; each of its level-2 children is an original carrying (id N). The
clone's SOURCE (rotated by the user, or the default) is paired with each
child's id N -- the key by which the server applies the chosen source."
  (let ((pairs nil)
        (parent-source nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (let ((level (org-current-level))
              (sexp (skg--metadata-sexp-at-point-or-nil)))
          (cond
           ((= level 1)
            ;; Always rebind on a level-1 headline -- even a
            ;; metadata-less or garbled one -- so it cannot leak a prior
            ;; clone-to-be's source to a later fork's child.
            (setq parent-source (and sexp (skg--node-source sexp))))
           ((and (= level 2) sexp parent-source)
            (let ((id (skg--node-id sexp)))
              (when id
                (push (cons id parent-source) pairs))))))
        (forward-line 1)))
    (nreverse pairs)))

(defun skg-approve-fork ()
  "Approve the forks listed in this *SKG Fork Confirmation* buffer:
extract each clone's chosen source, re-save the originating buffer with
the forks approved (carrying those sources), then kill the confirmation
buffer.

Interactively, first prompts for any clone source still at
`skg-fork-source-placeholder' (as the confirmation handler already did
when the buffer appeared -- this catches placeholders that survived,
e.g. after quitting those prompts). Refuses if any placeholder remains
anyway; the confirmation buffer is left open so you can set sources by
hand (C-c s s)."
  (interactive)
  (unless noninteractive
    (skg--fork-choose-placeholder-sources))
  (let ((origin skg--fork-origin-buffer)
        (fork-sources (skg--fork-sources-from-confirmation-buffer)))
    (unless (buffer-live-p origin)
      (error "The buffer that requested these forks is no longer open"))
    (when (seq-some (lambda (pair)
                      (string= (cdr pair) skg-fork-source-placeholder))
                    fork-sources)
      (user-error
       "Pick a source for each clone first: point on a clone-to-be headline, then C-c s s"))
    ;; The atom must survive to the re-save (which commits the fork; the
    ;; server then drops it on re-render), so suppress the kill-hook strip.
    (setq skg--fork-suppress-strip-on-kill t)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (current-buffer)))
    (with-current-buffer origin
      (skg-request-save-buffer t fork-sources))))

(defun skg-decline-fork ()
  "Decline the forks; nothing was written. Strip any lingering explicit
\(viewRequests fork) atom from the origin buffer -- otherwise the next save
of that buffer would silently re-fork -- then leave this confirmation
buffer open (it is navigable -- search it for relevant IDs).

Stripping is a no-op for an implicit (foreign) fork, whose origin headline
carries no fork atom."
  (interactive)
  (when (buffer-live-p skg--fork-origin-buffer)
    (with-current-buffer skg--fork-origin-buffer
      (skg-strip-fork-requests-in-buffer)))
  (skg-register-buffer
   (current-buffer) 'derived-report
   :lifecycle 'client-local
   :disposable nil
   :recipe '((kind . "fork-declined-report"))
   :last-fetched (skg-buffer-raw-text))
  (message
   "Fork declined; nothing was saved. This buffer is left open for reference."))

(defun skg--message-list-nonempty-p (message-list)
  "Return non-nil when MESSAGE-LIST has at least one message."
  (and (listp message-list)
       message-list))

(defun skg-messages-to-org-string (messages)
  "Convert MESSAGES (from the server) to org-buffer text.
Each message becomes its own headline."
  (if (listp messages)
      (mapconcat (lambda (message) (concat "* " message)) messages "\n")
    (concat "* " messages)))

(defun skg-errors-and-warnings-to-org-string (errors warnings)
  "Convert ERRORS and WARNINGS to one org buffer with two sections."
  (let ((sections nil))
    (when (skg--message-list-nonempty-p errors)
      (push (concat "* errors\n"
                    (mapconcat (lambda (message)
                                 (concat "** " message))
                               errors "\n"))
            sections))
    (when (skg--message-list-nonempty-p warnings)
      (push (concat "* warnings\n"
                    (mapconcat (lambda (message)
                                 (concat "** " message))
                               warnings "\n"))
            sections))
    (mapconcat #'identity (nreverse sections) "\n")))

(defalias 'skg-errors-to-org-string #'skg-messages-to-org-string)

(defun skg-handle-save-sexp (sexp-string)
  "Parse and handle save response s-exp.
Expected shape: ((content ...) (errors (...)) (warnings (...)))."
  (condition-case err
      (let* ((response (read sexp-string))
             (_session (skg-require-current-server-session response))
             (content-value (cadr (assoc 'content response)))
             (errors-list   (cadr (assoc 'errors response)))
             (warnings-list (cadr (assoc 'warnings response)))
             (authority (skg--view-authority-from-response response))
             (save-point-position
              (skg--save-point-position-from-response response)))
        (when content-value
          (skg-replace-buffer-with-new-content
           nil content-value save-point-position authority))
        (when skg--disk-conflict-resolution-in-progress
          (setq skg--disk-conflict-resolution-in-progress nil)
          (when (and content-value
                     (not (skg--message-list-nonempty-p errors-list)))
            (skg-clear-disk-client-conflict)))
        (when (or (skg--message-list-nonempty-p errors-list)
                  (skg--message-list-nonempty-p warnings-list))
          (skg-show-save-errors-and-warnings
           errors-list warnings-list content-value)))
    (error (skg-log 'error 'save "parsing save response: %S" err)
           (skg-log 'error 'save "sexp string was: %S" sexp-string))))

(defun skg--save-point-position-from-response (response)
  "Extract optional save point position from RESPONSE."
  (list :point-lines-below-focused-headline
        (skg--nat-from-response response
                                'point-lines-below-focused-headline)
        :point-column
        (skg--nat-from-response response
                                'point-column)
        :point-screen-lines-below-window-start
        (skg--nat-from-response response
                                'point-screen-lines-below-window-start)))

(defun skg--nat-from-response (response key)
  "Extract natural number KEY from RESPONSE, accepting strings or ints."
  (let ((entry (assoc key response)))
    (when entry
      (let ((value (cadr entry)))
        (cond
         ((natnump value) value)
         ((and (stringp value)
               (string-match-p "\\`[0-9]+\\'" value))
          (string-to-number value)))))))

(defun skg--view-authority-from-response (response)
  "Return application authority plist from RESPONSE, or nil when absent."
  (when (assoc 'client-application-token response)
    (append
     (list :server-session-id
           (skg-require-current-server-session response)
           :graph-generation
           (skg--nat-from-response response 'graph-generation)
           :presentation-generation
           (skg--nat-from-response response 'presentation-generation)
           :server-revision
           (skg--nat-from-response response 'server-revision)
           :application-token
           (skg--nat-from-response response 'client-application-token))
     (when (assoc 'root-ids response)
       (list :root-ids
             (mapcar (lambda (id) (format "%s" id))
                     (or (cadr (assoc 'root-ids response)) nil)))))))

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content
                                                      &optional
                                                      save-point-position
                                                      authority)
  "Replace the current buffer contents with NEW-CONTENT from Rust.
After inserting content, folds marked headlines, removes fold markers,
moves point to focused headline, and removes focus marker.
When AUTHORITY includes base fields, this is a background installation and
every component of the registered application record must still match."
  (when (and authority skg--buffer-record)
    (let ((response-session (plist-get authority :server-session-id))
          (expected-token (plist-get authority :expected-application-token))
          (result-token (plist-get authority :application-token)))
      (when (and response-session
                 (not (equal response-session skg--server-session-id)))
        (error "Skg refused authority from a stale server session"))
      (when (and response-session
                 (not (equal response-session
                             (skg--buffer-record-server-session-id
                              skg--buffer-record))))
        (error "Skg buffer belongs to an earlier server session; reopen it"))
      (when (and (plist-get authority :client-buffer-id)
                 (not (equal (plist-get authority :client-buffer-id)
                             (skg--buffer-record-id skg--buffer-record))))
        (error "Skg server offer names a different client buffer"))
      (when (and (plist-member authority :view-uri)
                 (not (equal (plist-get authority :view-uri)
                             (skg--buffer-record-view-uri
                              skg--buffer-record))))
        (error "Skg view URI changed before application"))
      (when (and expected-token
                 (/= expected-token
                     (skg--buffer-record-application-token
                      skg--buffer-record)))
        (error "Skg application token changed before application"))
      (when (and expected-token result-token
                 (/= result-token (1+ expected-token)))
        (error "Skg server offer does not advance exactly one token"))
      (when (and (plist-member authority :base-server-revision)
                 (/= (plist-get authority :base-server-revision)
                     (skg--buffer-record-server-revision
                      skg--buffer-record)))
        (error "Skg server revision changed before application"))
      (when (and (plist-member authority :base-graph-generation)
                 (/= (plist-get authority :base-graph-generation)
                     (skg--buffer-record-graph-generation
                      skg--buffer-record)))
        (error "Skg graph generation changed before application"))
      (when (and (plist-member authority :base-presentation-generation)
                 (/= (plist-get authority :base-presentation-generation)
                     (skg--buffer-record-presentation-generation
                      skg--buffer-record)))
        (error "Skg presentation generation changed before application"))
      (when (and (plist-member authority :base-source-set)
                 (not (equal (plist-get authority :base-source-set)
                             (skg--buffer-record-source-set
                              skg--buffer-record))))
        (error "Skg source-set changed before application"))
      (when (and (plist-get authority :require-clean)
                 (skg-buffer-dirty-p))
        (error "Skg refuses to replace a dirty buffer"))
      (when (and result-token
                 (not expected-token)
                 (/= result-token
                     (1+ (skg--buffer-record-application-token
                          skg--buffer-record))))
        (error "Skg save response has an obsolete application token"))))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert new-content)
    (;; PITFALL: `erase-buffer' does NOT remove overlays — they collapse
     ;; but persist at the buffer boundaries. Fold overlays left over
     ;; from the previous save cycle will re-expand over freshly inserted
     ;; text, making some headings already folded here. We MUST unfold
     ;; before any metadata edit, or `skg-edit-metadata-at-point' will
     ;; call `delete-region' on a folded heading line, and org-fold's
     ;; `org-fold-core--fix-folded-region' will expand the deletion to
     ;; cover the hidden subtree — clobbering the root.
     org-fold-show-all)
    (progn
      ;; Process focus marker BEFORE fold markers, while the buffer is
      ;; guaranteed unfolded by the `org-fold-show-all' above.
      (skg-goto-focused-headline)
      (skg-remove-focused-marker))
    (save-excursion
      ;; Process folding markers (now safe — all metadata edits done).
      ;; Wrap in `save-excursion' because `skg-fold-marked-headlines'
      ;; leaves point on the last parent it folded; we need point to
      ;; stay on the focused headline set just above.
      (skg-fold-marked-headlines)
      (skg-remove-folded-markers))
    (skg--restore-save-point-position save-point-position)
    (setq skg--last-rendered-content new-content)
    (let ((next-token
           (or (plist-get authority :application-token)
               (1+ skg--application-token))))
      (setq skg--application-token next-token)
      (when skg--buffer-record
        (setf
         (skg--buffer-record-last-fetched skg--buffer-record) new-content
         (skg--buffer-record-last-fetched-sha256 skg--buffer-record)
         (skg--sha256-text new-content)
         (skg--buffer-record-application-token skg--buffer-record) next-token
         (skg--buffer-record-graph-generation skg--buffer-record)
         (or (plist-get authority :graph-generation)
             (skg--buffer-record-graph-generation skg--buffer-record))
         (skg--buffer-record-presentation-generation skg--buffer-record)
         (or (plist-get authority :presentation-generation)
             (skg--buffer-record-presentation-generation skg--buffer-record))
         (skg--buffer-record-server-revision skg--buffer-record)
         (or (plist-get authority :server-revision)
             (skg--buffer-record-server-revision skg--buffer-record))
         (skg--buffer-record-source-set skg--buffer-record)
         (or (plist-get authority :source-set)
             (skg--buffer-record-source-set skg--buffer-record)))
        (when (plist-member authority :root-ids)
          (setf (skg--buffer-record-root-ids skg--buffer-record)
                (plist-get authority :root-ids)))))
      (when authority
        (skg-observe-server-graph-generation
         (plist-get authority :graph-generation)))
    (setq skg--background-refresh-stale nil)
    (set-buffer-modified-p
     ;; Clear modified flag and re-register the one-shot hook
     ;; AFTER all buffer modifications are done.
     nil)
    (add-hook 'first-change-hook
              #'skg-warn-if-other-buffer-modified nil t)
    (message "Buffer updated with processed content from Rust")))

(defun skg--restore-save-point-position (save-point-position)
  "Restore point and window row from SAVE-POINT-POSITION, if available."
  (when save-point-position
    (let ((point-lines-below-focused-headline
           (plist-get save-point-position
                      :point-lines-below-focused-headline))
          (point-column
           (plist-get save-point-position
                      :point-column))
          (point-screen-lines-below-window-start
           (plist-get save-point-position
                      :point-screen-lines-below-window-start)))
      (when point-lines-below-focused-headline
        (skg--restore-point-below-focused-headline
         point-lines-below-focused-headline point-column))
      (when point-screen-lines-below-window-start
        (skg--recenter-current-buffer-window
         point-screen-lines-below-window-start)))))

(defun skg--restore-point-below-focused-headline (line-offset column)
  "Move LINE-OFFSET lines below the focused headline, then to COLUMN.
Assumes point starts on the focused headline.
The target line is clamped to the focused headline's own entry: if the
entry has shrunk so that LINE-OFFSET lines down would reach another
headline (or the buffer end), point lands on the entry's last line
instead of crossing into a different headline.
Within the target line, point is clamped to the line's end, so a
shortened line never spills point onto the following line.
COLUMN is a character offset from the line's start; nil means column 0."
  (let* ((entry-end ;; first position past the focused headline's entry
          (save-excursion
            (if (outline-next-heading)
                (line-beginning-position)
              (point-max))))
         (target-bol ;; start of the chosen line, clamped to the entry
          (save-excursion
            (forward-line line-offset)
            (when (>= (point) entry-end)
              ;; Entry shrank: fall back to the entry's last line.
              (goto-char entry-end)
              (forward-line -1))
            (line-beginning-position))))
    (goto-char (min (+ target-bol (or column 0))
                    (save-excursion
                      (goto-char target-bol)
                      (line-end-position))))))

(defun skg--recenter-current-buffer-window (screen-line)
  "Place point on SCREEN-LINE in a displayed window for this buffer."
  (let ((window (get-buffer-window (current-buffer) t)))
    (when window
      (with-selected-window window
        (recenter screen-line)))))

(defun skg-big-nonfatal-message (buffer-name message-text content)
  "Display CONTENT in BUFFER-NAME and show MESSAGE-TEXT in minibuffer."
  (with-current-buffer (skg-acquire-generated-buffer buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert content)
      (skg--org-mode-with-options)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (skg-register-buffer
     (current-buffer) 'durable-report
     :lifecycle 'client-local :disposable nil
     :recipe `((kind . "message-report") (requested-name . ,buffer-name))
     :last-fetched (skg-buffer-raw-text))
    (display-buffer (current-buffer))
    (message "%s" message-text)))

(defun skg-show-save-errors-and-warnings
    (errors warnings content-present)
  "Show ERRORS and WARNINGS from a save response in one org buffer."
  (let* ((has-errors (skg--message-list-nonempty-p errors))
         (has-warnings (skg--message-list-nonempty-p warnings))
         (buffer-name
          (cond
           ((and has-errors has-warnings) "*SKG Save Errors and Warnings*")
           (has-errors "*SKG Save Errors - Inconsistencies Found*")
           (t "*SKG Save Warnings*")))
         (message-text
          (cond
           ((and has-errors has-warnings)
            "Save reported errors and warnings")
           (has-errors
            "Save failed - errors shown in *SKG Save Errors - Inconsistencies Found*")
           (content-present
            "Save succeeded with warnings - see *SKG Save Warnings*")
           (t
            "Save reported warnings")))
         (content
          (skg-errors-and-warnings-to-org-string errors warnings)))
    (skg-big-nonfatal-message
     buffer-name message-text content)))

(defun skg--background-collateral-offer-handler (tcp-proc payload)
  "Apply a revision-checked background view offer, then ACK or reject it."
  (let* ((response (read payload))
         (_session (skg-require-current-server-session response))
         (operation-id
          (skg--atom-string (cadr (assoc 'operation-id response))))
         (uri (skg--atom-string (cadr (assoc 'view-uri response))))
         (graph-generation (cadr (assoc 'graph-generation response)))
         (presentation-generation
          (cadr (assoc 'presentation-generation response)))
         (base-revision
          (cadr (assoc 'viewforest-base-revision response)))
         (base-graph-generation
          (cadr (assoc 'view-base-graph-generation response)))
         (base-presentation-generation
          (cadr (assoc 'view-base-presentation-generation response)))
         (expected-token
          (cadr (assoc 'expected-client-application-token response)))
         (result-token
          (cadr (assoc 'resulting-client-application-token response)))
         (result-revision
          (cadr (assoc 'resulting-server-revision response)))
         (client-buffer-id
          (skg--atom-string (cadr (assoc 'client-buffer-id response))))
         (base-source-set
          (skg--atom-string
           (cadr (assoc 'view-base-source-set response))))
         (result-source-set
          (skg--atom-string
           (cadr (assoc 'resulting-source-set response))))
         (content (cadr (assoc 'content response)))
         (needs-authorization
          (skg--atom-string
           (cadr (assoc 'needs-authorization response))))
         (render-error
          (skg--atom-string (cadr (assoc 'render-error response))))
         (buf (or (skg-find-buffer-by-id client-buffer-id)
                  (and uri (skg-find-buffer-by-uri uri))))
         (applied nil)
         (authorized nil)
         (client-token expected-token))
    (dolist (warning (or (cadr (assoc 'warnings response)) nil))
      (message "SKG background refresh warning: %s" warning))
    (cond
     ((equal needs-authorization "true")
      (ding)
      (let ((prompt (or (cadr (assoc 'prompt response))
                        "Include protected text in this background refresh?")))
        (if (buffer-live-p buf)
            (setq authorized (y-or-n-p (concat prompt " ")))
          (message "SKG discarded authorization for a closed view: %s"
                   prompt))))
     (render-error
      (ding)
      (message "SKG background refresh failed for %s: %s"
               (or uri "closed view") render-error))
     ((not (buffer-live-p buf)) nil)
     ((with-current-buffer buf (skg-buffer-dirty-p))
      (with-current-buffer buf
        (setq skg--background-refresh-stale
              `((operation-id . ,operation-id)
                (graph-generation . ,graph-generation))))
      (message "SKG left modified buffer %s stale; save or refresh it explicitly"
               (buffer-name buf)))
     ((stringp content)
      (with-current-buffer buf
        (condition-case err
            (progn
              (skg-replace-buffer-with-new-content
               nil content nil
               (list :client-buffer-id client-buffer-id
                     :server-session-id
                     (skg-require-current-server-session
                      response
                      (and (buffer-live-p buf)
                           (buffer-local-value 'skg--buffer-record buf)))
                     :view-uri uri
                     :base-server-revision base-revision
                     :base-graph-generation base-graph-generation
                     :base-presentation-generation
                     base-presentation-generation
                     :base-source-set base-source-set
                     :expected-application-token expected-token
                     :graph-generation graph-generation
                     :presentation-generation presentation-generation
                     :server-revision result-revision
                     :application-token result-token
                     :source-set result-source-set
                     :require-clean t))
              (setq applied t
                    client-token skg--application-token))
          (error
           (setq skg--background-refresh-stale
                 `((operation-id . ,operation-id)
                   (graph-generation . ,graph-generation)
                   (reason . ,(error-message-string err))))
           (skg-log 'error 'save
                    "background offer refused for %s: %S"
                    (buffer-name) err))))))
    (skg-register-response-handler 'collateral-applied #'ignore t)
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       (append
        `((request . "apply collateral")
          (operation-id . ,operation-id)
          (view-uri . ,uri)
          (applied . ,(if applied "true" "nil"))
          (authorized . ,(if authorized "true" "nil"))
          (graph-generation . ,graph-generation)
          (presentation-generation . ,presentation-generation)
          (viewforest-base-revision . ,base-revision)
          (resulting-server-revision . ,result-revision)
          (view-base-graph-generation . ,base-graph-generation)
          (view-base-presentation-generation
           . ,base-presentation-generation)
          (expected-client-application-token . ,expected-token)
          (resulting-client-application-token . ,result-token)
          (client-token . ,client-token)
          (view-base-source-set . ,base-source-set)
          (resulting-source-set . ,result-source-set))
        (when client-buffer-id
          `((client-buffer-id . ,client-buffer-id)))))
      "\n"))))

(skg-register-server-push-handler
 'collateral-view #'skg--background-collateral-offer-handler)

(provide 'skg-request-save)
