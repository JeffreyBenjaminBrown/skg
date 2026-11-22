;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Redraw a whole view from the server's new text -- the
;;; saved view after a save, or another view a save or a rerender
;;; changed -- keeping each window's point and the folds where they
;;; were. The entry points: 'skg--apply-streamed-view-update', which the
;;; save's collateral-view and the rerender's rerender-view messages
;;; share; 'skg--replace-buffer-keeping-viewnode-points-and-folds', for
;;; a redraw the user did not ask for; and
;;; 'skg-replace-buffer-with-new-content', for the saved buffer, whose
;;; point 'skg--current-save-point-position' measured before the save
;;; and 'skg--restore-save-point-position' puts back. Rewriting only
;;; the headlines a save changed is 'skg-redraw-delta.el's; choosing
;;; between that and a whole redraw, for the saved buffer, is
;;; 'skg-request-save.el's ('skg--redraw-saved-buffer').

(require 'cl-lib)
(require 'org)
(require 'skg-buffer)       ; for skg-find-buffer-by-view-id, skg--capture-clean-baseline
(require 'skg-length-prefix) ; for skg-tell-answer-not-applied
(require 'skg-lock-buffers) ; for skg--unlock-after-save
(require 'skg-log)
(require 'skg-org-fold)     ; for skg-fold-headlines-by-atoms
(require 'skg-redraw-delta) ; for the viewnode-ID headlines and atom stripping

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
                    "%s handler error: %S" handler-name err)
           (skg-tell-answer-not-applied
            handler-name err
            (if (eq log-category 'save)
                "the server may have saved, but a view was not redrawn"
              "a view was not redrawn")))))

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

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content
                                                      &optional
                                                      save-point-position)
  "Replace the current buffer contents with NEW-CONTENT from Rust.
The buffer gets NEW-CONTENT's stripped form, in one insertion, so that
the modification hooks run once rather than once per atom; the fold
atoms the stripping removed become org folds
\(`skg-fold-headlines-by-atoms'), and point goes to the headline that
held the focus atom (else to the buffer's start), then by
SAVE-POINT-POSITION."
  (let* ((inhibit-read-only t)
         (skg--inhibit-dirty-view-confirmation t)
         (stripped-and-atom-headlines
          (skg--stripped-text-and-atom-headlines new-content)))
    (erase-buffer)
    (insert (car stripped-and-atom-headlines))
    (;; PITFALL: `erase-buffer' does not remove overlays — they collapse
     ;; but persist at the buffer boundaries. Fold overlays left over
     ;; from the previous save cycle will re-expand over freshly inserted
     ;; text, making some headlines already folded here. We MUST unfold
     ;; before folding by the atoms.
     org-fold-show-all)
    (let ((atom-headlines
           (mapcar (lambda (headline)
                     (cons (+ (point-min) (car headline)) (cdr headline)))
                   (cdr stripped-and-atom-headlines))))
      (skg-fold-headlines-by-atoms atom-headlines)
      (goto-char (or (car (cl-find-if
                           (lambda (headline)
                             (member "focused" (cdr headline)))
                           atom-headlines))
                     (point-min))))
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

(provide 'skg-redraw-whole-view)
