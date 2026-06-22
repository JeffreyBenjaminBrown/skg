;;; -*- lexical-binding: t; -*-

(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-org-fold)
(require 'skg-focus)
(require 'skg-metadata)
(require 'skg-buffer)
(require 'skg-lock-buffers)

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

(defun skg-request-save-buffer (&optional fork-approved fork-sources)
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
  (skg--confirm-save-despite-other-unsaved)
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
           (buffer-contents (buffer-string))
           (request-s-exp (concat (prin1-to-string
                                   (skg--save-request-sexp
                                    skg-view-uri
                                    save-point-position
                                    fork-approved
                                    fork-sources))
                                  "\n"))
           (content-bytes (encode-coding-string buffer-contents 'utf-8))
           (content-length (length content-bytes))
           (header (format "Content-Length: %d\r\n\r\n" content-length)))
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

      (skg--begin-stream "save")

      ;; Lock ALL skg content-view buffers immediately, before sending.
      ;; This eliminates the race window between the send and the
      ;; server's early response.
      (skg--lock-all-skg-buffers)

      ;; Register handlers in the dispatch map
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
       (lambda (_tcp-proc payload)
         (skg--save-result-handler save-buffer payload))
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
         (skg--fork-confirmation-handler save-buffer payload))
       nil)

      (skg-lp-reset)

      ;; Send the request line first
      (process-send-string tcp-proc request-s-exp)

      ;; Send the length-prefixed buffer contents
      (process-send-string tcp-proc header)
      (process-send-string tcp-proc buffer-contents))))

(defun skg--save-request-sexp (view-uri save-point-position
                                        &optional fork-approved fork-sources)
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
     (list (list 'fork-sources fork-sources)))))

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
its view URI.  Shared by the save (collateral-view) and rerender (rerender-view)
streams; LOG-CATEGORY and HANDLER-NAME label any error."
  (condition-case err
      (let* ((response (read payload))
             (uri (cadr (assoc 'view-uri response)))
             (content (cadr (assoc 'content response)))
             (buf (skg-find-buffer-by-uri uri)))
        (when buf
          (with-current-buffer buf
            (skg--unlock-after-save)
            (skg-replace-buffer-with-new-content nil content))))
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
  (setq skg-response-handler-map
        (assoc-delete-all 'collateral-view skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'save-relax-lock skg-response-handler-map))
  ;; The fork-confirmation alternative did not fire; drop it. It is
  ;; non-one-shot, so no pending-count adjustment is needed.
  (setq skg-response-handler-map
        (assoc-delete-all 'fork-confirmation skg-response-handler-map))
  (skg--end-stream)
  (unwind-protect
      (progn
        (skg--unlock-all-save-locked)
        (with-current-buffer save-buffer
          (skg-handle-save-sexp payload)))
    (skg--unlock-all-save-locked)) )

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
read-only confirmation buffer (which lists the nodes that would be
forked) and, interactively, ask whether to approve.

Terminal, like `skg--save-result-handler': remove the streaming handlers
AND the unfired save-result one-shot (decrementing the pending count for
it), end the stream, and unlock."
  (setq skg-response-handler-map
        (assoc-delete-all 'collateral-view skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'save-relax-lock skg-response-handler-map))
  (setq skg-response-handler-map
        (assoc-delete-all 'fork-confirmation skg-response-handler-map))
  (when (assoc 'save-result skg-response-handler-map)
    ;; save-result was registered one-shot but will never fire; remove it
    ;; and decrement the pending count it bumped, or the next save hangs.
    (setq skg-response-handler-map
          (assoc-delete-all 'save-result skg-response-handler-map))
    (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (condition-case err
      (let* ((response (read payload))
             (content (cadr (assoc 'content response)))
             (to-minibuffer (cadr (assoc 'to-minibuffer response)))
             (confirm-buf (skg--show-fork-confirmation content save-buffer)))
        (when to-minibuffer (message "%s" to-minibuffer))
        (unless noninteractive
          ;; In batch (tests) the caller drives skg-approve-fork /
          ;; skg-decline-fork directly; interactively, ask now.
          (with-current-buffer confirm-buf
            (if (yes-or-no-p "Fork the listed node(s)? ")
                (skg-approve-fork)
              (skg-decline-fork)))))
    (error (skg-log 'error 'save "fork-confirmation handler error: %S" err))))

(defun skg--show-fork-confirmation (content save-buffer)
  "Show CONTENT in the *SKG Fork Confirmation* buffer, recording
SAVE-BUFFER as its origin, and return the buffer. The buffer is a
navigable content view (so id-push / search work). It is editable so the
user can rotate each clone's source (C-c s s on the clone-to-be parent),
but it is NOT an ordinary save target: skg-view-uri is left nil (tripping
the nil-view-uri save guard) and C-x C-s is rebound to refuse, because a
stray normal save of its id-less clone-to-be parents would create bare
nodes. Only C-c C-c (approve) and C-c C-k (decline) act on it."
  (let ((buf (get-buffer-create "*SKG Fork Confirmation*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content ""))
        (skg-content-view-mode)
        (when (fboundp 'heralds-minor-mode) (heralds-minor-mode))
        (goto-char (point-min)))
      ;; nil view-uri: not a registered view, and the ordinary-save guard
      ;; rejects M-x skg-request-save-buffer on it.
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
      (set-buffer-modified-p nil))
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
buffer."
  (interactive)
  (let ((origin skg--fork-origin-buffer)
        (fork-sources (skg--fork-sources-from-confirmation-buffer)))
    (unless (buffer-live-p origin)
      (error "The buffer that requested these forks is no longer open"))
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
             (content-value (cadr (assoc 'content response)))
             (errors-list   (cadr (assoc 'errors response)))
             (warnings-list (cadr (assoc 'warnings response)))
             (save-point-position
              (skg--save-point-position-from-response response)))
        (when content-value
          (skg-replace-buffer-with-new-content
           nil content-value save-point-position))
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

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content
                                                      &optional
                                                      save-point-position)
  "Replace the current buffer contents with NEW-CONTENT from Rust.
After inserting content, folds marked headlines, removes fold markers,
moves point to focused headline, and removes focus marker."
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
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert content)
      (skg--org-mode-with-options)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (display-buffer buffer-name)
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

(provide 'skg-request-save)
