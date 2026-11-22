;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-org-fold)
(require 'skg-metadata)
(require 'skg-buffer)
(require 'skg-lock-buffers)
(require 'skg-messages)
(require 'skg-processed) ; for skg--processed-summary-message
(require 'skg-redraw-delta)
(require 'skg-redraw-whole-view)
(require 'skg-save-confirmations) ; the handlers of the alternatives to save-result

(defun skg-request-save-buffer (&optional approved-forks fork-skgrepos
                                          approved-hoist-pids
                                          strandeds-review
                                          delete-from)
  "Send the current buffer contents to Rust for processing.
Before sending, adds 'folded' atoms to folded headlines and 'focused' atom to current headline.
The server sends three LP messages around the save:
  1. save-lock: acknowledges the client's early broad lock.
  2. save-relax-lock: narrows the lock once the checked buffer and
     collateral set are known, plus a collateral-view per rerendered buffer.
  3. save-result: the saved buffer's final content (+ errors/warnings).
All skg buffers are locked immediately.  The broad acknowledgement does not
unlock client-only views; save-relax-lock is the first narrowing point.

If the save edited any foreign node, the server reads that as a request
to fork (clone) it and -- unless FORK-APPROVED is non-nil -- replies with
a `fork-confirmation' message instead of `save-result', skgsave-committing
nothing. `skg--fork-confirmation-handler' then shows the confirmation
buffer and offers `skg-approve-fork' (re-save with FORK-APPROVED) /
`skg-decline-fork'.

STRANDEDS-REVIEW, when non-nil, is (reviewed . TRIPLES): the user has
reviewed the relationships a home skgrepo move strands (see
`skg--strandeds-confirmation-handler'), and TRIPLES, each
(RELATION RECORDER MEMBER), are the ones to reset to default.

DELETE-FROM, when non-nil, is \"visible\" or \"all\": where the save's
deletes may strip the deleted IDs, as the user chose after a
`delete-from-confirmation' (see `skg--delete-from-confirmation-handler').
It rides this save and its retries only, never cached."
  (interactive)
  (unless skg-view-id
    (error "Cannot save: skg-view-id is nil in buffer '%s' (content-view-mode=%s). Re-open the view."
           (buffer-name)
           (if (derived-mode-p 'skg-content-view-mode) "on" "off")))
  (skg--begin-stream "save")
  (skg--lock-all-skg-buffers)
  (condition-case err
      (skg--send-save-buffer
       approved-forks fork-skgrepos approved-hoist-pids
       strandeds-review delete-from)
    (error
     (skg--cancel-locally-failed-save)
     (signal (car err) (cdr err)))))

(defun skg--send-save-buffer (approved-forks fork-skgrepos
                                            approved-hoist-pids
                                            strandeds-review
                                            delete-from)
  "Serialize and send a save once `skg--stream-in-progress' is set and
the buffers are locked."
  (when (org-before-first-heading-p)
    ;; Rather than complain, save as if point were at the first headline.
    (goto-char (point-min))
    (outline-next-heading))
  (let ((save-point-position
         (skg--current-save-point-position)))
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (save-buffer (current-buffer))
           (saved-view-id skg-view-id)
           (buffer-contents
            (skg--buffer-snapshot-with-save-metadata))
           (baseline (skg--saved-buffer-baseline))
           (other-view-buffer-snapshots
            (skg--other-view-buffer-snapshots save-buffer))
           (wire-content
            (skg--serialize-save-envelope
             buffer-contents baseline other-view-buffer-snapshots))
           (request-s-exp (concat (prin1-to-string
                                   (skg--save-request-sexp
                                    skg-view-id
                                    save-point-position
                                    approved-forks
                                    fork-skgrepos
                                    approved-hoist-pids
                                    strandeds-review
                                    delete-from))
                                  "\n"))
           (content-bytes (encode-coding-string wire-content 'utf-8))
           (content-length (length content-bytes))
           (header (format "Content-Length: %d\r\n\r\n" content-length)))
      ;; Register handlers in the dispatch map
      (skg-register-response-handler
       'save-lock
       (lambda (_tcp-proc payload)
         (skg--broad-save-lock-handler payload))
       t)
      ;; save-relax-lock carries the post-preparation keep-set: collateral
      ;; targets plus dirty conflict-check inputs. Buffers locked early that
      ;; are absent can unlock. The saved buffer stays
      ;; locked (skg--unlock-non-collateral-buffers keeps saved-view-id) until
      ;; save-result. Registered non-one-shot (like collateral-view) so it does
      ;; not add to skg-lp--pending-count: an *invalid* save errors before the
      ;; server reaches the point that emits save-relax-lock, so a one-shot
      ;; count would leak (never decremented) and hang the next save's wait.
      ;; save-result removes it.
      (skg-register-response-handler
       'save-relax-lock
       (lambda (_tcp-proc payload)
         (skg--save-relax-lock-handler saved-view-id payload))
       nil)
      (skg-register-response-handler
       'collateral-view
       (lambda (_tcp-proc payload)
         (skg--collateral-view-handler payload))
       nil) ;; non-one-shot: fires for each streamed collateral view
      (skg-register-response-handler
       'save-result
       (lambda (_tcp-proc payload)
         (skg--save-result-handler save-buffer payload))
       t)
      ;; fork-confirmation: the alternative terminal message to
      ;; save-result. The server sends exactly one of the two. Registered
      ;; non-one-shot (does not bump skg-lp--pending-count); whichever
      ;; terminal handler fires removes the other (and the fork handler
      ;; decrements the count for the unfired save-result one-shot), so
      ;; the pending count balances either way.
      (skg-register-response-handler
       'fork-confirmation
       (lambda (_tcp-proc payload)
         (skg--fork-confirmation-handler
          save-buffer payload strandeds-review delete-from))
       nil)
      ;; Another alternative terminal: the save's deletes would strip
      ;; records the skgrepo restriction keeps out. Ask where to delete
      ;; from, and retry this same save with the choice.
      (skg-register-response-handler
       'delete-from-confirmation
       (lambda (_tcp-proc payload)
         (skg--delete-from-confirmation-handler
          save-buffer payload approved-forks fork-skgrepos
          approved-hoist-pids strandeds-review))
       nil)
      ;; Another alternative terminal: a home skgrepo move would strand
      ;; relationships. Retry this same save with the user's review.
      (skg-register-response-handler
       'strandeds-confirmation
       (lambda (_tcp-proc payload)
         (skg--strandeds-confirmation-handler
          save-buffer payload approved-forks fork-skgrepos
          approved-hoist-pids delete-from))
       nil)
      ;; The other alternative terminal. It carries no title/body text; after
      ;; approval, retry this same save with the exact listed PIDs.
      (skg-register-response-handler
       'telescope-hoist-confirmation
       (lambda (_tcp-proc payload)
         (skg--telescope-hoist-confirmation-handler
          save-buffer payload approved-forks fork-skgrepos
          strandeds-review delete-from))
       nil)
      ;; The terminal message of a save whose rerender is withheld. The
      ;; save was written; approval asks for a rerender of the views it
      ;; names, not for the save again.
      (skg-register-response-handler
       'overPrivateText-telescope-confirmation
       (lambda (_tcp-proc payload)
         (skg--save-text-release-confirmation-handler payload))
       nil)

      (skg-lp-reset)

      ;; Send the request line first
      (process-send-string tcp-proc request-s-exp)

      ;; Send the length-prefixed buffer contents
      (process-send-string tcp-proc header)
      (process-send-string tcp-proc wire-content))))

(defun skg--saved-buffer-baseline ()
  "The current buffer's baseline, `skg-clean-baseline', which every save
carries.  Every view a client can save has one, so its absence is an
error, not something to send."
  (unless (stringp skg-clean-baseline)
    (error "Cannot save: this view has no baseline (the text the server last drew).  Reopen it"))
  (substring-no-properties skg-clean-baseline))

(defun skg--serialize-save-envelope (saved-buffer baseline other-view-buffer-snapshots)
  "Serialize SAVED-BUFFER, its BASELINE and OTHER-VIEW-BUFFER-SNAPSHOTS for
the Rust server."
  ;; Emacs prints its empty list as the atom `nil', while the Rust protocol
  ;; requires an explicit list for other-views.  Serialize that one empty
  ;; value as `()'.  Ignore user printer limits: truncation would corrupt the
  ;; protocol rather than merely shorten display output.
  (let ((print-length nil)
        (print-level nil))
    (concat
     "((saved-buffer " (prin1-to-string saved-buffer) ") "
     "(baseline " (prin1-to-string baseline) ") "
     "(other-views "
     (if other-view-buffer-snapshots
         (prin1-to-string other-view-buffer-snapshots)
       "()")
     "))")))

(defun skg--transient-save-atoms ()
  "The atoms a save writes into the current buffer's headline metadata,
as a list of (POSITION ATOM...), one per headline that gets any, last
headline first: `(skg folded)' on a headline hidden inside a folded
ancestor, else `(skg bodyFolded)' on one whose entry alone is hidden
\(`skg-headline-body-hidden-p'), and then `(skg focused)' on the
headline holding point."
  (let ((focused (save-excursion (org-back-to-heading t) (point)))
        (headline-atoms nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (org-at-heading-p)
          (let ((atoms
                 (append (cond ((invisible-p (point)) '((skg folded)))
                               ((skg-headline-body-hidden-p)
                                '((skg bodyFolded))))
                         (when (= (point) focused) '((skg focused))))))
            (when atoms
              (push (cons (point) atoms) headline-atoms))))
        (forward-line 1)))
    headline-atoms))

(defun skg--buffer-snapshot-with-save-metadata ()
  "Return the current buffer's text, without text properties, with the
transient save atoms (`skg--transient-save-atoms') merged into its
headlines' metadata.  The atoms are written into a copy, so the buffer
itself, its folds, and its modification hooks are untouched.  Text
properties are an Emacs presentation concern: `prin1-to-string' would
emit them as #(\"...\" ...) syntax, which the Rust reader does not
accept."
  (let ((headline-atoms (skg--transient-save-atoms))
        (headline-regexp outline-regexp)
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (setq-local outline-regexp headline-regexp)
      ;; Last headline first, so that each rewrite leaves the positions
      ;; of the headlines above it valid.
      (dolist (entry headline-atoms)
        (goto-char (car entry))
        (dolist (atom (cdr entry))
          (skg-edit-metadata-at-point atom)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun skg--other-view-buffer-snapshots (saved-buffer)
  "Return the structured states of every live view except SAVED-BUFFER."
  (let (result)
    (dolist (buffer (buffer-list) (nreverse result))
      (when (and (not (eq buffer saved-buffer))
                 (buffer-local-value 'skg-view-id buffer))
        (with-current-buffer buffer
          (push
           (if (buffer-modified-p)
               `((view-id ,skg-view-id)
                 (dirty true)
                 (baseline ,(if (stringp skg-clean-baseline)
                                `(present ,(substring-no-properties
                                            skg-clean-baseline))
                              'unavailable))
                 (current ,(buffer-substring-no-properties
                            (point-min) (point-max))))
             `((view-id ,skg-view-id) (dirty false)))
           result))))))

(defconst skg--save-response-types-not-one-shot
  '(save-relax-lock collateral-view
    fork-confirmation strandeds-confirmation delete-from-confirmation
    telescope-hoist-confirmation overPrivateText-telescope-confirmation)
  "The response types `skg--send-save-buffer' registers without ONE-SHOT:
the streamed ones, which can fire any number of times, and the
alternatives to save-result, which the server sends instead of it. The
dispatcher never removes these, so the message that ends the save does.
The other two, save-lock and save-result, are one-shot.")

(defun skg--cancel-locally-failed-save ()
  "Unwind handlers, pending counts, stream state and locks after local failure."
  (dolist (response-type '(save-lock save-result))
    (when (assoc response-type skg-response-handler-map)
      (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count)))))
  (dolist (response-type
           (append '(save-lock save-result)
                   skg--save-response-types-not-one-shot))
    (setq skg-response-handler-map
          (assoc-delete-all response-type skg-response-handler-map)))
  (skg--end-stream)
  (skg--unlock-all-save-locked))

(defun skg--end-save-without-save-result ()
  "End a save that an alternative to save-result ended (a confirmation
message): remove the save's handlers, end the stream, and unlock."
  (dolist (response-type skg--save-response-types-not-one-shot)
    (setq skg-response-handler-map
          (assoc-delete-all response-type skg-response-handler-map)))
  (when (assoc 'save-result skg-response-handler-map)
    ;; save-result was registered one-shot but will never fire; remove it
    ;; and decrement the pending count it bumped, or the next save hangs.
    (setq skg-response-handler-map
          (assoc-delete-all 'save-result skg-response-handler-map))
    (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
  (skg--end-stream)
  (skg--unlock-all-save-locked))

(defun skg--save-request-sexp (view-id save-point-position
                                        &optional approved-forks fork-skgrepos
                                        approved-hoist-pids
                                        strandeds-review
                                        delete-from)
  "Build the save-buffer request sexp. When FORK-APPROVED is non-nil,
include (approved-forks . \"true\") so the server skgsave-commits any forks it
finds instead of returning a fork-confirmation. FORK-REPOS, when
non-nil, is an alist ((N . REPO) ...) pairing each forked node's id
with the owned repo the user chose for its clone; it rides out as the
field (fork-repos ((N . REPO) ...)). STRANDEDS-REVIEW, when non-nil,
rides out as (strandeds-reviewed . \"true\") and, unless it chose
none, (reset-strandeds (RELATION RECORDER MEMBER) ...). DELETE-FROM,
when non-nil, rides out as (delete-from . DELETE-FROM). A
buffer of the diff view names itself, by `skg-diffView-buffer-kind',
as (diff-view-buffer . NAME), and a view shown from a perspective
other than its root homes names it, by `skg-perspective', as
(perspective . NAME)."
  (append
   `((request . "save buffer")
     (view-id . ,view-id))
   (when skg-diffView-buffer-kind
     `((diff-view-buffer . ,skg-diffView-buffer-kind)))
   (when skg-perspective
     `((perspective . ,skg-perspective)))
   `((point-lines-below-focused-headline
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
   (when approved-forks
     '((approved-forks . "true")))
   (when fork-skgrepos
     (list (list 'fork-repos fork-skgrepos)))
   (when approved-hoist-pids
     `((approved-hoist-pids ,@approved-hoist-pids)))
   (when strandeds-review
     '((strandeds-reviewed . "true")))
   (when (cdr strandeds-review)
     `((reset-strandeds ,@(cdr strandeds-review))))
   (when delete-from
     `((delete-from . ,delete-from)))))

(defun skg--broad-save-lock-handler (payload)
  "Validate a broad save-lock acknowledgement without narrowing locks."
  (condition-case err
      (let* ((response (read payload))
             (lock-entry (assoc 'lock-views response)))
        (unless (and lock-entry (listp (cadr lock-entry)))
          (error "Malformed broad save-lock payload")))
    (error
     ;; A malformed acknowledgement cannot safely authorize any unlock.
     (skg-log 'error 'save "broad save-lock handler error: %S" err)
     (skg-tell-answer-not-applied
      "save-lock" err "views stay locked until the save ends")) ))

(defun skg--save-relax-lock-handler (saved-view-id payload)
  "Narrow broad save locks to SAVED-VIEW-ID and the view IDs in PAYLOAD."
  (condition-case err
      (let* ((response (read payload))
             (lock-entry (assoc 'lock-views response)))
        (unless (and lock-entry (listp (cadr lock-entry)))
          (error "Malformed save-relax-lock payload"))
        (skg--unlock-non-collateral-buffers
         saved-view-id
         (mapcar (lambda (view-id)
                   (cond ((stringp view-id) view-id)
                         ((symbolp view-id) (symbol-name view-id))
                         (t (error "Malformed lock view ID: %S" view-id))))
                 (cadr lock-entry))))
    (error
     ;; Retain every lock: an incomplete or malformed keep-set cannot safely
     ;; identify a buffer whose checked contents are no longer needed.
     (skg-log 'error 'save "save-relax-lock handler error: %S" err)
     (skg-tell-answer-not-applied
      "save-relax-lock" err "views stay locked until the save ends")) ))

(defun skg--collateral-view-handler (payload)
  "Handle one streamed collateral-view update.
Unlocks and updates the buffer for the given view ID, unless that
buffer has unsaved edits: the server never redraws a dirty view, and
this guard keeps a client from losing those edits if it did."
  (skg--apply-streamed-view-update payload 'save "collateral-view" t))

(defun skg--save-result-handler (save-buffer payload)
  "Handle the full save-result LP message (tagged with response-type).
Removes the collateral-view handler, unlocks all save-locked buffers,
then processes the save response.
Unlock must happen before `skg-handle-save-sexp' because
`skg-replace-buffer-with-new-content' calls erase-buffer + insert,
which would trigger overlay modification-hooks if still present."
  ;; The alternatives to save-result did not fire; drop them. They are
  ;; non-one-shot, so no pending-count adjustment is needed.
  (dolist (response-type skg--save-response-types-not-one-shot)
    (setq skg-response-handler-map
          (assoc-delete-all response-type skg-response-handler-map)))
  (skg--end-stream)
  (unwind-protect
      (progn
        (skg--unlock-all-save-locked)
        (with-current-buffer save-buffer
          (skg-handle-save-sexp payload))
        (skg--finish-pending-fork-result save-buffer payload))
    (skg--unlock-all-save-locked)) )

(defun skg-handle-save-sexp (sexp-string)
  "Parse and handle save response s-exp.
Expected shape: ((content ...) (errors (...)) (warnings (...)))."
  (condition-case err
      (let* ((response (read sexp-string))
             (content-value (cadr (assoc 'content response)))
             (errors-list   (cadr (assoc 'errors response)))
             (warnings-list (cadr (assoc 'warnings response)))
             (save-point-position
              (skg--save-point-position-from-response response)))
        (when content-value
          (skg--redraw-saved-buffer
           content-value
           (assq 'changed-headlines response)
           save-point-position))
        (when (or (skg--message-list-nonempty-p errors-list)
                  (skg--message-list-nonempty-p warnings-list))
          (skg-show-save-errors-and-warnings
           errors-list warnings-list content-value))
        (let ((summary (assq 'processed-summary response)))
          (when summary ;; the saved buffer is one of the diff view's
            (message "%s" (skg--processed-summary-message summary))))
        (let ((hides (cadr (assq 'hides response))))
          (when (skg--message-list-nonempty-p hides)
            (message "%s" (skg--save-hides-message
                           hides (current-message))))))
    (error (skg-log 'error 'save "parsing save response: %S" err)
           (skg-log 'error 'save "sexp string was: %S" sexp-string)
           (skg-tell-answer-not-applied "save-result" err))))

(defun skg--save-hides-message (hides &optional shown)
  "The echo-area text after a save whose answer's '(hides ...)' is
HIDES: what the save hid and stopped hiding, and each removal from a
forker that hid nothing, one line each, as the server words them
\('docs/api-and-formats.org', \"Save buffer\"). SHOWN, the message the
save already showed, if any, stays above them."
  (mapconcat #'identity
             (append (when (and shown (not (string-empty-p shown)))
                       (list shown))
                     hides)
             "\n"))

(defun skg--redraw-saved-buffer (new-content
                                  changed-headlines-entry
                                  save-point-position)
  "Show NEW-CONTENT in the current buffer. When CHANGED-HEADLINES-ENTRY,
the response's '(changed-headlines (V ...))', is present, rewrite only
the headlines it names (`skg--patch-buffer-with-delta'), leaving folds
and point as they are; otherwise, or when the patch fails, redraw the
whole buffer, restoring folds and point from the atoms and
SAVE-POINT-POSITION."
  (if (and changed-headlines-entry
           (skg--patch-buffer-with-delta
            new-content (cadr changed-headlines-entry)))
      (progn
        (skg-log 'debug 'save "redraw: patched %d headlines"
                 (length (cadr changed-headlines-entry)))
        (set-buffer-modified-p nil)
        (skg--capture-clean-baseline)
        (when (fboundp 'skg-link-annotations-refresh)
          (skg-link-annotations-refresh))
        (message "Buffer updated with processed content from Rust"))
    (skg-log 'debug 'save "redraw: whole buffer (%s)"
             (if changed-headlines-entry "the patch failed" "no delta"))
    (skg-replace-buffer-with-new-content
     nil new-content save-point-position)))

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

(provide 'skg-request-save)
