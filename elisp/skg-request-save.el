;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'skg-keymaps-and-aliases) ; for skg-report-mode-map
(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-org-fold)
(require 'skg-focus)
(require 'skg-metadata)
(require 'skg-buffer)
(require 'skg-lock-buffers)
(require 'skg-processed) ; for skg--processed-summary-message
(require 'skg-redraw-delta)
(declare-function skg--request-rerender-views "skg-request-rerender-all-views"
                  (view-ids approved-pids))

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
  (let ((focused-had-metadata ;; Whether the focused headline already has metadata. Storing this lets us clean up the bare (skg) that removal leaves behind.
         (save-excursion
           (org-back-to-heading t)
           (looking-at "\\*+ (skg")))
        (save-point-position
         (skg--current-save-point-position)))
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (save-buffer (current-buffer))
           (saved-view-id skg-view-id)
           (buffer-contents
            (skg--buffer-snapshot-with-save-metadata focused-had-metadata))
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

(defun skg--buffer-snapshot-with-save-metadata (focused-had-metadata)
  "Return current text with transient save atoms, restoring the buffer.
FOCUSED-HAD-METADATA records whether atom removal can leave a bare skg
form.  The saved buffer's lock is suspended only during this synchronous
internal edit; it is restored before any request is sent."
  (let ((was-save-locked skg--save-lock-overlay)
        (skg--inhibit-dirty-view-confirmation t)
        buffer-snapshot)
    (when was-save-locked
      (skg--unlock-after-save))
    (unwind-protect
        (progn
          (skg-add-folded-metadata)
          (skg-add-focused-metadata)
          ;; Text properties are an Emacs presentation concern.  If retained,
          ;; `prin1-to-string' emits #("..." ...) syntax, which is not part of
          ;; the shared S-expression protocol and is parsed as extra fields by
          ;; the Rust reader.
          (setq buffer-snapshot
                (buffer-substring-no-properties (point-min) (point-max))))
      (skg-remove-focused-metadata)
      (skg-remove-folded-metadata)
      (unless focused-had-metadata
        (skg-strip-bare-skg-at-focused-headline))
      (when was-save-locked
        (skg--lock-for-save)))
    buffer-snapshot))

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

(defun skg--cancel-locally-failed-save ()
  "Unwind handlers, pending counts, stream state and locks after local failure."
  (dolist (response-type '(save-lock save-result))
    (when (assoc response-type skg-response-handler-map)
      (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count)))))
  (dolist (response-type
           '(save-lock save-relax-lock collateral-view save-result
             fork-confirmation strandeds-confirmation
             delete-from-confirmation
             telescope-hoist-confirmation
             overPrivateText-telescope-confirmation))
    (setq skg-response-handler-map
          (assoc-delete-all response-type skg-response-handler-map)))
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

(defun skg--current-save-point-position (&optional window)
  "WHAT IT DOES: Return point position data that should survive the save's rerender:
- text-line offset: relative to the headline owning point (the save
  marks it focused)
- column: the character offset of point within its line
- screen-line offset: the screen line of WINDOW (default: a window
  showing the buffer) that point is on, counting the window's top
  line as 0, as `recenter' takes it
WHY: A character offset from buffer start is not stable enough: saving can make branches appear or disappear above point. The focused headline is already preserved by metadata, so record point relative to that headline and to the window top. The within-line column is stable regardless of what happens above, so it is recorded as a plain offset."
  (let ((point-line (line-number-at-pos (point) t))
        (window (or window (get-buffer-window (current-buffer) t))))
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
                    (save-excursion ;; the start of point's screen line,
                      ;; so that the line point is on is not counted
                      (vertical-motion 0 window)
                      (point))
                    nil
                    window))
            0))))

(defun skg-strip-bare-skg-at-focused-headline ()
  "Remove bare (skg) from the current headline if that is its only metadata.
Used after atom removal to clean up headlines that had no metadata
before the add/remove cycle."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at "\\(\\*+ \\)(skg) ")
      (replace-match "\\1"))))

(defun skg--broad-save-lock-handler (payload)
  "Validate a broad save-lock acknowledgement without narrowing locks."
  (condition-case err
      (let* ((response (read payload))
             (lock-entry (assoc 'lock-views response)))
        (unless (and lock-entry (listp (cadr lock-entry)))
          (error "Malformed broad save-lock payload")))
    (error
     ;; A malformed acknowledgement cannot safely authorize any unlock.
     (skg-log 'error 'save "broad save-lock handler error: %S" err)) ))

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
     (skg-log 'error 'save "save-relax-lock handler error: %S" err)) ))

(defun skg--apply-streamed-view-update (
         payload log-category handler-name &optional keep-unsaved-edits)
  "Apply one streamed view update from PAYLOAD: unlock and replace the buffer for
its view ID.  Shared by the save (collateral-view) and rerender (rerender-view)
streams; LOG-CATEGORY and HANDLER-NAME label any error.
If KEEP-UNSAVED-EDITS is non-nil, a buffer with unsaved edits is left
as it is, and named in the echo area."
  (condition-case err
      (let* ((response (read payload))
             (view-id (cadr (assoc 'view-id response)))
             (content (cadr (assoc 'content response)))
             (buf (skg-find-buffer-by-view-id view-id)))
        (when buf
          (if (and keep-unsaved-edits (buffer-modified-p buf))
              (message "skg: not redrawn by the save, having unsaved edits: %s"
                       (buffer-name buf))
            (with-current-buffer buf
              (skg--unlock-after-save)
              (skg--replace-buffer-keeping-viewnode-points-and-folds
               content)))))
    (error (skg-log 'error log-category
                    "%s handler error: %S" handler-name err))))

(defun skg--replace-buffer-keeping-viewnode-points-and-folds (new-content)
  "Replace the current buffer with NEW-CONTENT, a redraw the user did
not ask for, keeping point, each window's point and scroll, and the
folds, where they were.
The server draws such a redraw with the folds of the view's last save,
since folds reach it only in a save. So the fold state of each headline
here is recorded by viewnode ID, and decides the fold atoms of
NEW-CONTENT (`skg--content-with-fold-atoms'); what only a headline new
in NEW-CONTENT decides keeps the server's.
A save marks the headline at point focused, and restores
point relative to it ('skg--restore-save-point-position'); here the
server drew the view on its own, so its focused headline is the one
the last save left. Instead each point is recorded as its headline's
viewnode ID, which a rerender keeps ('docs/api-and-formats.org',
\"(vnid X)\"), with the offsets a save measures, and restored by them.
A point whose viewnode is gone goes to the start of the nearest
headline that remains. A point whose headline has no viewnode ID (one
typed since the last save, which a save held back marks focused) goes
to the focused headline, as the server drew it."
  (let* ((old-headlines (skg--viewnode-id-headlines))
         (fold-states-by-viewnode-id
          (skg--fold-states-by-viewnode-id old-headlines))
         (buffer-point-position
          (skg--viewnode-point-position (point) nil old-headlines))
         (window-point-positions
          (mapcar (lambda (window)
                    (cons window
                          (skg--viewnode-point-position
                           (window-point window) window old-headlines)))
                  (get-buffer-window-list (current-buffer) nil t))))
    (skg-replace-buffer-with-new-content
     nil (skg--content-with-fold-atoms new-content
                                       fold-states-by-viewnode-id))
    (let ((focused-headline-start (point))
          (headline-starts
           (skg--headline-starts-by-viewnode-id
            (mapcar #'car old-headlines))))
      (skg--goto-viewnode-point-position
       buffer-point-position headline-starts focused-headline-start)
      (dolist (window-and-position window-point-positions)
        (with-selected-window (car window-and-position)
          (skg--goto-viewnode-point-position
           (cdr window-and-position)
           headline-starts focused-headline-start))))))

(defun skg--fold-states-by-viewnode-id (headlines)
  "A table from the viewnode ID of each of HEADLINES
\(`skg--viewnode-id-headlines' of the current buffer) to the fold state
of its headline (`skg--fold-state-of-headline-at-point')."
  (let ((fold-states (make-hash-table :test 'equal)))
    (save-excursion
      (dolist (headline headlines fold-states)
        (goto-char (cadr headline))
        (puthash (car headline)
                 (skg--fold-state-of-headline-at-point)
                 fold-states)))))

(defun skg--content-with-fold-atoms (content fold-states-by-viewnode-id)
  "CONTENT, a view's text, with the fold atoms that FOLD-STATES-BY-VIEWNODE-ID
\(`skg--fold-states-by-viewnode-id' of the buffer it will replace)
decides. A headline's 'folded' atom says its parent's fold hides it,
so its parent's state decides it (`skg--folded-atom-decision'); its
'bodyFolded' atom, its own (`skg--bodyFolded-atom-decision'). An atom
whose deciding headline the table lacks, being new in CONTENT, is kept
as the server drew it. Other lines, and the focus atom, are kept."
  (let ((lines (vconcat (split-string content "\n")))
        (ancestors nil)) ;; (LEVEL . VIEWNODE-ID), nearest first
    (cl-loop
     for (headline . later-headlines)
     on (skg--headline-levels-and-viewnode-ids lines)
     do (cl-destructuring-bind (index level viewnode-id) headline
          (while (and ancestors (>= (caar ancestors) level))
            (pop ancestors))
          (when viewnode-id
            (aset lines index
                  (skg--line-with-fold-atoms
                   (aref lines index)
                   (delq nil
                         (list
                          (skg--folded-atom-decision
                           ancestors
                           (and ancestors
                                (cdar ancestors)
                                (gethash (cdar ancestors)
                                         fold-states-by-viewnode-id)))
                          (skg--bodyFolded-atom-decision
                           (gethash viewnode-id fold-states-by-viewnode-id)
                           (and later-headlines
                                (> (nth 1 (car later-headlines))
                                   level))))))))
          (push (cons level viewnode-id) ancestors)))
    (mapconcat #'identity lines "\n")))

(defun skg--headline-levels-and-viewnode-ids (lines)
  "Each headline among LINES, a vector of a view's lines, as
\(INDEX LEVEL VIEWNODE-ID), in order; VIEWNODE-ID is nil for a
headline without one."
  (let ((case-fold-search nil)
        (headlines nil))
    (dotimes (index (length lines) (nreverse headlines))
      (let ((line (aref lines index)))
        (when (string-match "\\`\\(\\*+\\) " line)
          (push (list index
                      (length (match-string 1 line))
                      (and (string-match
                            skg--viewnode-id-at-headline-start-regexp line)
                           (match-string 1 line)))
                headlines))))))

(defun skg--folded-atom-decision (has-parent parent-fold-state)
  "Whether a headline is 'folded', as (\"folded\" . BOOLEAN), given
whether it HAS-PARENT and the PARENT-FOLD-STATE that parent had
\(`skg--fold-state-of-headline-at-point'): it is when its parent hid
its children or was hidden, and is not when its parent showed them.
Nil, leaving the server's atom, for a top-level headline or one whose
parent had no state, being new."
  (when has-parent
    (pcase parent-fold-state
      ((or 'hidden 'collapsed) '("folded" . t))
      ((or 'open 'bodyHidden) '("folded" . nil)))))

(defun skg--bodyFolded-atom-decision (fold-state has-children)
  "Whether a headline is 'bodyFolded', as (\"bodyFolded\" . BOOLEAN),
given the FOLD-STATE it had (`skg--fold-state-of-headline-at-point')
and whether it HAS-CHILDREN now: it is when only its body was hidden,
or when it was collapsed and its children are gone, so that the fold
hides its body still. Nil, leaving the server's atom, when it had no
state, being new."
  (pcase fold-state
    ('bodyHidden '("bodyFolded" . t))
    ('collapsed (cons "bodyFolded" (not has-children)))
    ((or 'hidden 'open) '("bodyFolded" . nil))))

(defun skg--line-with-fold-atoms (line decisions)
  "LINE, a headline line beginning with its viewnode ID, with each atom
DECISIONS names, as (ATOM . BOOLEAN), removed, and those whose BOOLEAN
is non-nil placed after the viewnode ID."
  (if (null decisions)
      line
    (string-match skg--viewnode-id-at-headline-start-regexp line)
    (let* ((viewnode-id-end (match-end 0))
           (without-decided-atoms
            (skg--without-fold-and-focus-atoms
             line (string-search "(skg" line) (mapcar #'car decisions))))
      (concat (substring without-decided-atoms 0 viewnode-id-end)
              (mapconcat (lambda (decision) (concat " " (car decision)))
                         (cl-remove-if-not #'cdr decisions) "")
              (substring without-decided-atoms viewnode-id-end)))))

(defun skg--viewnode-point-position (position window headlines)
  "Where POSITION in the current buffer stands, as a plist: the save's
measures (`skg--current-save-point-position'), the screen line in
WINDOW, or none when WINDOW is nil; :viewnode-ids, the viewnode IDs of
HEADLINES (`skg--viewnode-id-headlines' of the current buffer), the
one owning POSITION first and the rest by their distance in lines from
POSITION, nearest first; and :owner-viewnode-id, the owning one's, nil
when it has none. Nil when POSITION precedes every headline."
  (save-excursion
    (goto-char position)
    (unless (org-before-first-heading-p)
      (let* ((owner-start (save-excursion (org-back-to-heading t)
                                          (point)))
             (point-line (line-number-at-pos position))
             (owner-id (car (cl-find owner-start headlines
                                     :key #'cadr)))
             (others-nearest-first
              (sort (copy-sequence
                     (cl-remove owner-start headlines :key #'cadr))
                    (lambda (a b)
                      (< (abs (- (nth 2 a) point-line))
                         (abs (- (nth 2 b) point-line))))))
             (measures (skg--current-save-point-position window)))
        (append (list :viewnode-ids
                      (append (when owner-id (list owner-id))
                              (mapcar #'car others-nearest-first))
                      :owner-viewnode-id owner-id)
                (if window
                    measures
                  (plist-put measures
                             :point-screen-lines-below-window-start nil)))))))

(defun skg--goto-viewnode-point-position (point-position
                                           headline-starts
                                           focused-headline-start)
  "Move point to POINT-POSITION (`skg--viewnode-point-position'), and,
when it carries a screen line, put point on that line of the selected
window: to its offsets below its own headline if HEADLINE-STARTS (a
table from viewnode ID to headline start) holds that, else to the
start of the first of its other headlines that it holds. A position
whose headline had no viewnode ID, or none of whose headlines remain,
goes to FOCUSED-HEADLINE-START."
  (let* ((owner-id (plist-get point-position :owner-viewnode-id))
         (surviving-id
          (and owner-id
               (cl-find-if (lambda (id) (gethash id headline-starts))
                           (plist-get point-position :viewnode-ids)))))
    (if (not surviving-id)
        (goto-char focused-headline-start)
      (goto-char (gethash surviving-id headline-starts))
      (skg--restore-save-point-position
       (if (equal surviving-id owner-id)
           point-position
         (list :point-lines-below-focused-headline 0
               :point-column 0
               :point-screen-lines-below-window-start
               (plist-get point-position
                          :point-screen-lines-below-window-start)))))))

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
  (setq skg-response-handler-map
        (assoc-delete-all 'collateral-view skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'save-relax-lock skg-response-handler-map))
  ;; The fork-confirmation alternative did not fire; drop it. It is
  ;; non-one-shot, so no pending-count adjustment is needed.
  (setq skg-response-handler-map
        (assoc-delete-all 'fork-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'strandeds-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'delete-from-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'telescope-hoist-confirmation
                          skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'overPrivateText-telescope-confirmation
                          skg-response-handler-map))
  (skg--end-stream)
  (unwind-protect
      (progn
        (skg--unlock-all-save-locked)
        (with-current-buffer save-buffer
          (skg-handle-save-sexp payload))
        (skg--finish-pending-fork-result save-buffer payload))
    (skg--unlock-all-save-locked)) )

(defconst skg-fork-repo-placeholder "PICK-A-REPO"
  "Sentinel repo the server pre-fills for a clone-to-be whose repo
the user has not specified (in the saved metadata or a prior round).
`skg--fork-choose-placeholder-repos' prompts for a replacement per
carrying clone (the user can also set one with C-c s s);
`skg-approve-fork' refuses while any remains. Must match
FORK_REPO_PLACEHOLDER in server/from_text/fork.rs.")

(defvar-local skg--fork-source-buffer nil
  "In a fork-confirmation buffer, the source buffer whose save raised the
forks. `skg-approve-fork' re-saves it (approved); `skg-decline-fork'
leaves things untouched.")

(defvar-local skg--fork-suppress-strip-on-kill nil
  "When non-nil, `skg--fork-confirmation-on-kill' does not strip the
source's fork atom. `skg-approve-fork' sets it before killing this
buffer, because its re-save still needs the atom to skgsave-commit the fork.")

(defvar-local skg--fork-delete-from nil
  "In a fork-confirmation buffer, the delete-from choice the forking save
carried, which `skg-approve-fork' carries again.")

(defvar-local skg--fork-strandeds-review nil
  "In a fork-confirmation buffer, the strandeds review the forking save
carried, for `skg-approve-fork' to carry again.")

(defvar-local skg--pending-fork-result nil
  "On a fork's source buffer, while the approved re-save is pending, the
alist ((N . SKGREPO) ...) it carries: each forked node's ID and the
owned skgrepo its clone goes into.  The save-result handler reports the
save's outcome against them.")

(defun skg--fork-confirmation-on-kill ()
  "`kill-buffer-hook' for a fork-confirmation buffer: dismissing it
without approving strips the lingering (viewRequests fork) atom from the
source buffer, so the next save does not silently re-fork. This covers
killing the buffer directly (C-x k, q, etc.); `skg-decline-fork' already
strips explicitly, and `skg-approve-fork' suppresses this (its re-save
needs the atom, and the server drops it on re-render). Stripping is
idempotent, so a redundant call after a decline is a harmless no-op."
  (unless skg--fork-suppress-strip-on-kill
    (when (buffer-live-p skg--fork-source-buffer)
      (with-current-buffer skg--fork-source-buffer
        (skg-strip-fork-requests-in-buffer)))))

(defun skg--forks-phrase (fork-skgrepos)
  "Describe FORK-SKGREPOS, an alist ((N . SKGREPO) ...), as in
\"1 node into skgrepo A\" or \"2 nodes into skgrepos A, B\"."
  (let ((count (length fork-skgrepos))
        (skgrepos (delete-dups (mapcar #'cdr fork-skgrepos))))
    (format "%d %s into %s %s"
            count (if (= count 1) "node" "nodes")
            (if (cdr skgrepos) "skgrepos" "skgrepo")
            (string-join skgrepos ", "))))

(defun skg--close-fork-confirmation (confirmation source fork-skgrepos)
  "Close CONFIRMATION once its forks are approved, and record
FORK-SKGREPOS on SOURCE as its pending fork result.  Each window
showing CONFIRMATION is quit as by `quit-window': a window
`display-buffer' made for it is deleted, and one it borrowed shows
its earlier buffer again, so no split is left unexplained.  The echo
area says the save is under way."
  (dolist (window (get-buffer-window-list confirmation nil t))
    (when (window-live-p window)
      (quit-restore-window window)))
  (with-current-buffer source
    (setq skg--pending-fork-result fork-skgrepos))
  (let ((kill-buffer-query-functions nil))
    (kill-buffer confirmation))
  (message "Fork approved; saving %s..." (skg--forks-phrase fork-skgrepos)))

(defun skg--finish-pending-fork-result (save-buffer payload)
  "Report the outcome of SAVE-BUFFER's approved fork save, from its
save-result PAYLOAD.  Success goes to the echo area, followed by any
hides the save reports.  A failure says so there too; its errors, and
any warnings, are in the buffer `skg-handle-save-sexp' showed.  A result
that cannot be read gets a buffer of its own."
  (when (buffer-live-p save-buffer)
    (let ((fork-skgrepos
           (buffer-local-value 'skg--pending-fork-result save-buffer)))
      (when fork-skgrepos
        (with-current-buffer save-buffer
          (setq skg--pending-fork-result nil))
        (let ((forks (skg--forks-phrase fork-skgrepos)))
          (condition-case err
              (let* ((response (read payload))
                     (errors (cadr (assoc 'errors response)))
                     (warnings (cadr (assoc 'warnings response)))
                     (hides (cadr (assq 'hides response))))
                (if (skg--message-list-nonempty-p errors)
                    (message "Forking %s failed; the errors are shown in their own buffer."
                             forks)
                  (message "%s"
                           (skg--save-hides-message
                            hides
                            (format "Forked %s%s." forks
                                    (if (skg--message-list-nonempty-p warnings)
                                        ", with warnings (shown in their own buffer)"
                                      ""))))))
            (error
             (skg-big-nonfatal-message
              "*SKG Fork Result*"
              (format "Forking %s: could not read the save result." forks)
              (format "* Forking %s: could not read the save result.\n%S\n"
                      forks err)))))))))

(defun skg--fork-confirmation-handler (save-buffer payload &optional strandeds-review
                                                     delete-from)
  "Handle a `fork-confirmation' LP message: the save edited foreign
node(s) and was not pre-approved, so nothing was skgsave-committed. Show the
confirmation buffer (which lists the nodes that would be forked) and,
interactively, prompt for any clone repo not yet specified, then ask
whether to approve.

Terminal, like `skg--save-result-handler': remove the streaming handlers
and the unfired save-result one-shot (decrementing the pending count for
it), end the stream, and unlock. STRANDEDS-REVIEW and DELETE-FROM are the
review and the delete-from choice the save carried, which the approving
re-save carries again."
  (setq skg-response-handler-map
        (assoc-delete-all 'collateral-view skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'save-relax-lock skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'fork-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'strandeds-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'delete-from-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'telescope-hoist-confirmation
                          skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'overPrivateText-telescope-confirmation
                          skg-response-handler-map))
  (when (assoc 'save-result skg-response-handler-map)
    ;; save-result was registered one-shot but will never fire; remove it
    ;; and decrement the pending count it bumped, or the next save hangs.
    (setq skg-response-handler-map
          (assoc-delete-all 'save-result skg-response-handler-map))
    (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (let ((confirm-buf
         ;; The condition-case guards only response parsing/display.
         ;; The interactive flow below runs outside it: a refusal
         ;; (user-error) from `skg-approve-fork' must reach the user,
         ;; not the log -- nesting it here used to swallow the
         ;; \"pick a repo first\" refusal, so approving with a
         ;; placeholder skgrepo silently did nothing.
         (condition-case err
             (let* ((response (read payload))
                    (content (cadr (assoc 'content response)))
                    (to-minibuffer (cadr (assoc 'to-minibuffer response)))
                    (buf (skg--show-fork-confirmation content save-buffer)))
               (with-current-buffer buf
                 (setq skg--fork-strandeds-review strandeds-review)
                 (setq skg--fork-delete-from delete-from))
               (when to-minibuffer (message "%s" to-minibuffer))
               buf)
           (error
            (skg-log 'error 'save "fork-confirmation handler error: %S" err)
            nil))))
    (when (and confirm-buf (not noninteractive))
      ;; In batch (tests) the caller drives skg-approve-fork /
      ;; skg-decline-fork directly; interactively, ask now. Quitting
      ;; (C-g) any prompt leaves the confirmation buffer open: set
      ;; skgrepos with C-c s s and approve with C-c C-c, or decline
      ;; with C-c C-k.
      (with-current-buffer confirm-buf
        (skg--fork-choose-placeholder-repos)
        (if (yes-or-no-p "Fork the listed node(s)? ")
            (skg-approve-fork)
          (skg-decline-fork))))))

;;; Delete-from confirmation: under a skgrepo restriction, the save's deletes
;;; would strip records the skgrepo restriction keeps out. The user chooses
;;; whether those go too.

(defun skg--delete-from-confirmation-handler
    (save-buffer payload approved-forks fork-skgrepos
                 approved-hoist-pids strandeds-review)
  "Handle a `delete-from-confirmation' message for SAVE-BUFFER: records the
skgrepo restriction keeps out name a graphnode the save deletes, and
nothing was skgsave-committed. Ask \"Delete from: (c)ancel, (v)isible
records only, (a)ll owned records\", and re-issue the same save with the
choice, carrying the other approvals along. Cancel saves nothing."
  (dolist (response-type
           '(collateral-view save-relax-lock fork-confirmation
             strandeds-confirmation delete-from-confirmation
             telescope-hoist-confirmation
             overPrivateText-telescope-confirmation))
    (setq skg-response-handler-map
          (assoc-delete-all response-type skg-response-handler-map)))
  (when (assoc 'save-result skg-response-handler-map)
    (setq skg-response-handler-map
          (assoc-delete-all 'save-result skg-response-handler-map))
    (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (condition-case err
      (let* ((response (read payload))
             (count (cadr (assoc 'count response)))
             (prompt (or (cadr (assoc 'prompt response))
                         "Records the skgrepo restriction keeps out name a deleted graphnode.")))
        (if noninteractive
            (message "%s restricted record(s) name a deleted graphnode; nothing was saved"
                     count)
          (let ((delete-from
                 (skg--delete-from-of-choice
                  (read-char-choice
                   (concat prompt
                           " Delete from: (c)ancel, (v)isible records only, (a)ll owned records ")
                   '(?c ?v ?a)))))
            (if (not delete-from)
                (message "Nothing was saved.")
              (with-current-buffer save-buffer
                (skg-request-save-buffer
                 approved-forks fork-skgrepos approved-hoist-pids
                 strandeds-review delete-from))))))
    (error
     (skg-log 'error 'save "delete-from-confirmation handler error: %S" err))))

(defun skg--delete-from-of-choice (choice)
  "The delete-from field's value for CHOICE, a key of the delete-from
prompt: \"visible\" for ?v, \"all\" for ?a, nil (cancel) otherwise."
  (pcase choice
    (?v "visible")
    (?a "all")
    (_ nil)))

;;; Strandeds confirmation: a home skgrepo move would leave relationships at their
;;; old, more private skgrepos. The server decides which
;;; (server/from_text/strandeds.rs); the user chooses which to reset to default.

(defun skg--strandeds-confirmation-handler
    (save-buffer payload approved-forks fork-skgrepos
                 approved-hoist-pids &optional delete-from)
  "Handle a `strandeds-confirmation' message for SAVE-BUFFER: the save's home
skgrepo moves would strand the relationships it lists, and nothing was
skgsave-committed. Show them, ask which to reset to default, and re-issue the
same save with that review, carrying the other approvals along."
  (dolist (response-type
           '(collateral-view save-relax-lock fork-confirmation
             strandeds-confirmation delete-from-confirmation
             telescope-hoist-confirmation
             overPrivateText-telescope-confirmation))
    (setq skg-response-handler-map
          (assoc-delete-all response-type skg-response-handler-map)))
  (when (assoc 'save-result skg-response-handler-map)
    (setq skg-response-handler-map
          (assoc-delete-all 'save-result skg-response-handler-map))
    (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (condition-case err
      (let* ((response (read payload))
             (strandeds (skg--strandeds-from-response response))
             (prompt (or (cadr (assoc 'prompt response))
                         "Reset the stranded relationships to default? ")))
        (if noninteractive
            (message "%d relationship(s) would be stranded; nothing was saved"
                     (length strandeds))
          (skg--show-strandeds strandeds)
          (let ((review (skg--strandeds-review-from-choice
                         strandeds
                         (car (read-multiple-choice
                               prompt
                               '((?a "all" "reset every listed relationship to default")
                                 (?n "none" "save, leaving them where they are")
                                 (?c "choose" "ask about each")
                                 (?q "quit" "save nothing"))))
                         #'y-or-n-p)))
            (if (not review)
                (message "Nothing was saved.")
              (with-current-buffer save-buffer
                (skg-request-save-buffer
                 approved-forks fork-skgrepos approved-hoist-pids
                 review delete-from))))))
    (error
     (skg-log 'error 'save "strandeds-confirmation handler error: %S" err))))

(defun skg--strandeds-from-response (response)
  "The stranded relationships in a strandeds-confirmation RESPONSE, each a
plist (:relation :recorder :member :relRepo :default) of strings."
  (mapcar (lambda (entry)
            (let ((field (lambda (key)
                           (format "%s" (cadr (assoc key entry))))))
              (list :relation (funcall field 'relation)
                    :recorder (funcall field 'recorder)
                    :member   (funcall field 'member)
                    :relRepo  (funcall field 'relRepo)
                    :default  (funcall field 'default))))
          (cadr (assoc 'strandeds response))))

(defun skg--stranded-description (stranded)
  "One line describing STRANDED, a plist from `skg--strandeds-from-response'."
  (format "%s %s %s: stays at %s; its default is now %s"
          (plist-get stranded :recorder) (plist-get stranded :relation)
          (plist-get stranded :member) (plist-get stranded :relRepo)
          (plist-get stranded :default)))

(defun skg--show-strandeds (strandeds)
  "List STRANDEDS in the *SKG Stranded Relationships* buffer."
  (with-current-buffer (get-buffer-create "*SKG Stranded Relationships*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "These relationships would stay in their old, more private skgrepos:\n")
      (dolist (stranded strandeds)
        (insert "- " (skg--stranded-description stranded) "\n"))
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

(defun skg--strandeds-review-from-choice (strandeds choice ask)
  "The strandeds review CHOICE (a key from the strandeds prompt) makes of
STRANDEDS: (reviewed . TRIPLES), each TRIPLE (RELATION RECORDER MEMBER)
a relationship to reset to default; or nil, to save nothing. For ?c, ASK is
called with a prompt per relationship, and resets to default those it accepts."
  (let ((triple (lambda (stranded)
                  (list (plist-get stranded :relation)
                        (plist-get stranded :recorder)
                        (plist-get stranded :member)))))
    (pcase choice
      (?a (cons 'reviewed (mapcar triple strandeds)))
      (?n (list 'reviewed))
      (?c (cons 'reviewed
                (delq nil
                      (mapcar (lambda (stranded)
                                (when (funcall ask
                                               (format "Reset %s to default? "
                                                       (skg--stranded-description
                                                        stranded)))
                                  (funcall triple stranded)))
                              strandeds))))
      (_ nil))))

(defun skg--telescope-hoist-confirmation-handler
    (save-buffer payload approved-forks fork-skgrepos
                 &optional strandeds-review delete-from)
  "Handle the text-free terminal Hoist challenge for SAVE-BUFFER.
The server has skgsave-committed nothing.  On approval, reissue the same save with
the exact candidate PIDs; on Abort, leave the buffer and every .skg file
untouched.  FORK-APPROVED and FORK-REPOS survive if this challenge arose
on a retry that had already received fork authority, STRANDEDS-REVIEW
if it had already been reviewed, and DELETE-FROM if it had been chosen."
  (setq skg-response-handler-map
        (assoc-delete-all 'collateral-view skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'strandeds-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'delete-from-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'save-relax-lock skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'fork-confirmation skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'telescope-hoist-confirmation
                          skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'overPrivateText-telescope-confirmation
                          skg-response-handler-map))
  (when (assoc 'save-result skg-response-handler-map)
    (setq skg-response-handler-map
          (assoc-delete-all 'save-result skg-response-handler-map))
    (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
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
                 approved-forks fork-skgrepos approved-pids
                 strandeds-review delete-from))
            (message
             "Hoist aborted; nothing was saved. Repair the .skg sections manually."))))
    (error
     (skg-log 'error 'save
              "telescope-hoist-confirmation handler error: %S" err))))

(defun skg--save-text-release-confirmation-handler (payload)
  "Handle a save-rerender text release challenge, the terminal message
of a save whose rerender would show text from overPrivateText
telescopes under the skgrepo restriction. The save has been written,
but the server has sent none of the rerendered text; its challenge
names the views withheld, the saved one first. Approval asks the
server to rerender those views, approving the PIDs the challenge
lists (`skg--request-rerender-views'); the save is not sent again,
since it would apply a second time (creating a new root of the view
again, or refused because a container it gave a new child would be an
edited stale graphnode).
Declining leaves the buffers as they are."
  (dolist (response-type
           '(collateral-view save-relax-lock fork-confirmation
             strandeds-confirmation delete-from-confirmation
             telescope-hoist-confirmation overPrivateText-telescope-confirmation))
    (setq skg-response-handler-map
          (assoc-delete-all response-type skg-response-handler-map)))
  (when (assoc 'save-result skg-response-handler-map)
    (setq skg-response-handler-map
          (assoc-delete-all 'save-result skg-response-handler-map))
    (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (condition-case err
      (let* ((response (read payload))
             (approved-pids
              (mapcar (lambda (pid) (format "%s" pid))
                      (cadr (assoc 'pids response))))
             (withheld-view-ids
              (mapcar (lambda (view-id) (format "%s" view-id))
                      (cadr (assoc 'view-ids response))))
             (prompt (or (cadr (assoc 'prompt response))
                         "Display staged text from overPrivateText telescopes? ")))
        (if noninteractive
            (message "Saved, but protected rerender text was withheld for %s"
                     (mapconcat #'identity approved-pids ", "))
          (if (yes-or-no-p prompt)
              (skg--request-rerender-views withheld-view-ids approved-pids)
            (message
             "Save succeeded; protected rerender text remains withheld and buffers are unchanged."))))
    (error
     (skg-log 'error 'save
              "save text-release confirmation handler error: %S" err))))

(defun skg--fork-suggested-repo-above-point ()
  "Return the suggested repo named by the comment directly above the
headline at point, or nil. The server writes that comment above each
clone-to-be whose repo the user has not yet specified."
  (save-excursion
    (forward-line -1)
    (when (looking-at
           "^# Suggested repo for the clone below: \\(.+\\)$")
      (string-trim (match-string 1)))))

(defun skg--fork-choose-placeholder-repos ()
  "Prompt for an owned repo for each clone-to-be still carrying
`skg-fork-repo-placeholder', writing the choice into the buffer.
The server's suggested repo (the comment above the clone) is the
default; S-left/S-right cycle through the repos you own. A no-op
when every clone's repo is already specified -- notably when the
saved metadata itself specified it (the server then omits the
placeholder): no redundant ask."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (beginning-of-line)
      (let ((sexp (skg--metadata-sexp-at-point-or-nil)))
        (when (and (= (org-current-level) 1)
                   sexp
                   (equal (skg--node-repo sexp)
                          skg-fork-repo-placeholder))
          (let* ((title (nth 2 (skg-split-as-stars-metadata-title
                                (skg-get-current-headline-text))))
                 (suggested (skg--fork-suggested-repo-above-point))
                 (owned (skg--owned-repos))
                 (default (or suggested (car owned)))
                 (choice (skg--completing-read-with-cycle
                          (format "Repo for the clone \"%s\" (default %s): "
                                  title default)
                          owned nil t nil nil default nil owned)))
            (skg--change-repo-at-point
             (if (string-empty-p choice) default choice)))))
      (forward-line 1))))

(defun skg--show-fork-confirmation (content save-buffer)
  "Show CONTENT in the *SKG Fork Confirmation* buffer, recording
SAVE-BUFFER as its source, and return the buffer. The buffer is a
navigable content view (so id-push / search work). It is editable so
each clone's repo can be set (the handler's minibuffer prompts write
into it; C-c s s on a clone-to-be works too), but it is not an ordinary
save target: skg-view-id is left nil (tripping the nil-view-id save
guard) and C-x C-s is rebound to refuse, because a stray normal save of
its id-less clone-to-be parents would create bare nodes. Only C-c C-c
\(approve) and C-c C-k (decline) act on it."
  (let ((buf (get-buffer-create "*SKG Fork Confirmation*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content ""))
        (skg-content-view-mode)
        (setq mode-name ;; not "SKG": this is not a savable view
              "skg-fork-ask")
        (when (fboundp 'heralds-minor-mode) (heralds-minor-mode))
        (goto-char (point-min)))
      ;; nil view-id: not a registered view, and the ordinary-save guard
      ;; rejects M-x skg-request-save-buffer on it.
      (setq skg-view-id nil)
      (setq skg--fork-source-buffer save-buffer)
      (setq skg--fork-suppress-strip-on-kill nil)
      ;; Dismissing this buffer without approving (killing it directly,
      ;; not via C-c C-k) must still strip the source's fork atom.
      (add-hook 'kill-buffer-hook #'skg--fork-confirmation-on-kill nil t)
      ;; Copy the mode map first so these overrides stay buffer-local --
      ;; local-set-key mutates (current-local-map) in place, which is the
      ;; shared skg-content-view-mode-map; rebinding C-x C-s on the shared
      ;; map would break saving in every content view.
      (use-local-map (copy-keymap (current-local-map)))
      ;; C-c C-c skgsave-commits the forks; C-c C-k declines; C-x C-s refuses
      ;; (this buffer must not be saved as ordinary content).
      (local-set-key (kbd "C-c C-c") #'skg-approve-fork)
      (local-set-key (kbd "C-c C-k") #'skg-decline-fork)
      (local-set-key (kbd "C-x C-s") #'skg--fork-confirmation-refuse-save)
      (set-buffer-modified-p nil))
    (display-buffer buf)
    buf))

(defun skg--fork-confirmation-refuse-save ()
  "Refuse an ordinary save of the fork-confirmation buffer.
Its clone-to-be parents are id-less owned-repo nodes; saving them as
ordinary content would create bare nodes. Approve with C-c C-c (skgsave-commits
the forks) or decline with C-c C-k."
  (interactive)
  (user-error
   "This is the fork-confirmation buffer; use C-c C-c to approve or C-c C-k to decline"))

(defun skg--fork-repos-from-confirmation-buffer ()
  "Walk the fork-confirmation buffer; return an alist ((N . REPO) ...).
Each level-1 headline is a clone-to-be carrying (skgRepos (nodeRepo
REPO)) and no gnid; each of its level-2 children is an original
carrying (gnid N). The
clone's REPO (rotated by the user, or the default) is paired with each
child's id N -- the key by which the server applies the chosen repo."
  (let ((pairs nil)
        (parent-skgrepo nil))
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
            ;; clone-to-be's repo to a later fork's child.
            (setq parent-skgrepo (and sexp (skg--node-repo sexp))))
           ((and (= level 2) sexp parent-skgrepo)
            (let ((gnid (skg--node-id sexp)))
              (when gnid
                (push (cons gnid parent-skgrepo) pairs))))))
        (forward-line 1)))
    (nreverse pairs)))

(defun skg-approve-fork ()
  "Approve the fork operations listed in this *SKG Fork Confirmation* buffer:
extract each clone's chosen repo, close the confirmation, and re-save
the source buffer with the fork operations approved (carrying those
repos).  When the approved save finishes, the echo area says where the
forks went, or that the save failed
\(`skg--finish-pending-fork-result').

Interactively, first prompts for any clone repo still at
`skg-fork-repo-placeholder' (as the confirmation handler already did
when the buffer appeared -- this catches placeholders that survived,
e.g. after quitting those prompts). Refuses if any placeholder remains
anyway; the confirmation buffer is left open so you can set repos by
hand (C-c s s)."
  (interactive)
  (unless noninteractive
    (skg--fork-choose-placeholder-repos))
  (let ((source skg--fork-source-buffer)
        (strandeds-review skg--fork-strandeds-review)
        (delete-from skg--fork-delete-from)
        (fork-skgrepos (skg--fork-repos-from-confirmation-buffer)))
    (unless (buffer-live-p source)
      (error "The buffer that requested these forks is no longer open"))
    (when (seq-some (lambda (pair)
                      (string= (cdr pair) skg-fork-repo-placeholder))
                    fork-skgrepos)
      (user-error
       "Pick a repo for each clone first: point on a clone-to-be headline, then C-c s s"))
    ;; The atom must survive to the re-save (which skgsave-commits the fork; the
    ;; server then drops it on re-render), so suppress the kill-hook strip.
    (setq skg--fork-suppress-strip-on-kill t)
    (skg--close-fork-confirmation (current-buffer) source fork-skgrepos)
    (with-current-buffer source
      (skg-request-save-buffer t fork-skgrepos nil strandeds-review
                               delete-from))))

(defun skg-decline-fork ()
  "Decline the forks; nothing was written. Strip any lingering explicit
\(viewRequests fork) atom from the source buffer -- otherwise the next save
of that buffer would silently re-fork -- then leave this confirmation
buffer open (it is navigable -- search it for relevant IDs).

Stripping is a no-op for an implicit (foreign) fork, whose source headline
carries no fork atom."
  (interactive)
  (when (buffer-live-p skg--fork-source-buffer)
    (with-current-buffer skg--fork-source-buffer
      (skg-strip-fork-requests-in-buffer)))
  (message
   "Fork declined; nothing was saved. This buffer is left open for reference."))

(defun skg--message-list-nonempty-p (message-list)
  "Return non-nil when MESSAGE-LIST has at least one message."
  (and (listp message-list)
       message-list))

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
           (skg-log 'error 'save "sexp string was: %S" sexp-string))))

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

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content
                                                      &optional
                                                      save-point-position)
  "Replace the current buffer contents with NEW-CONTENT from Rust.
After inserting content, folds marked headlines, removes fold atoms,
moves point to focused headline, and removes focus atom."
  (let ((inhibit-read-only t)
        (skg--inhibit-dirty-view-confirmation t))
    (erase-buffer)
    (insert new-content)
    (;; PITFALL: `erase-buffer' does not remove overlays — they collapse
     ;; but persist at the buffer boundaries. Fold overlays left over
     ;; from the previous save cycle will re-expand over freshly inserted
     ;; text, making some headlines already folded here. We MUST unfold
     ;; before any metadata edit, or `skg-edit-metadata-at-point' will
     ;; call `delete-region' on a folded headline line, and org-fold's
     ;; `org-fold-core--fix-folded-region' will expand the deletion to
     ;; cover the hidden subtree — clobbering the root.
     org-fold-show-all)
    (progn
      ;; Process focus atom before fold atoms, while the buffer is
      ;; guaranteed unfolded by the `org-fold-show-all' above.
      (skg-goto-focused-headline)
      (skg-remove-focused-metadata))
    (save-excursion
      ;; Process folding atoms (now safe — all metadata edits done).
      ;; Wrap in `save-excursion' because `skg-fold-marked-headlines'
      ;; leaves point on the last parent it folded; we need point to
      ;; stay on the focused headline set just above.
      (skg-fold-marked-headlines)
      (skg-remove-folded-metadata))
    (skg--restore-save-point-position save-point-position)
    (set-buffer-modified-p nil)
    (skg--capture-clean-baseline)
    (when (fboundp 'skg-link-annotations-refresh)
      (skg-link-annotations-refresh)))
  (message "Buffer updated with processed content from Rust"))

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

(define-minor-mode skg-report-mode
  "Minor mode for read-only skg report buffers, such as an unknown node's
investigation and the import result: links in them can be followed,
and IDs used."
  :lighter " skg-report"
  :keymap skg-report-mode-map)

(defun skg-big-nonfatal-message (buffer-name message-text content)
  "Display CONTENT in BUFFER-NAME and show MESSAGE-TEXT in minibuffer.
Return the buffer."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert content)
      (skg--org-mode-with-options)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (display-buffer buffer-name)
    (message "%s" message-text)
    (current-buffer)))

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
