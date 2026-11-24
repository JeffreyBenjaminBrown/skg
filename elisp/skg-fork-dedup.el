;;; skg-fork-dedup.el --- Cover what a forks-folder member restates  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; A member of a forks folder often says what the folder's note says:
;;; its title, its body, or both, are the note's. Where so, overlays
;;; say it in the member's title, b being the note's ancestor letter
;;; as counted from the member (its grandparent, past the folder):
;;; - both: the title reads "<b unchanged>", and the lines under it,
;;;   up to the member's next headline, are hidden;
;;; - the title alone: the title reads "<b's title unchanged>";
;;; - the body alone: the title is shown, and the body reads
;;;   "<b's body unchanged>".
;;; Two empty bodies are equal. A body the view does not draw (a
;;; write-protected node's 'omittedBody') equals none.
;;; The buffer keeps the real text, which a save sends as it is; the
;;; overlays are drawing only. 'skg-fork-dedup-overlay', buffer-local,
;;; turns them on (the default) or off; 'skg-toggle-fork-dedup-overlay'
;;; (C-c v u) toggles it, so that the text can be read and edited.
;;; An edit that makes the two differ removes the overlay.
;;;
;;; The overlays are tagged (overlay-put ov 'skg-fork-dedup t) and found
;;; by that tag, as the herald overlays are ('heralds-minor-mode.el',
;;; PITFALL: ORPHANED OVERLAYS). This file draws only when called, and
;;; the herald machinery calls it: 'heralds-apply-to-buffer' draws them
;;; all ('skg-fork-dedup-refresh-buffer'), 'heralds-after-change'
;;; redraws those an edit can change ('skg-fork-dedup-refresh-region'),
;;; and so does the delta redraw ('skg-redraw-delta.el',
;;; 'skg-fork-dedup-refresh-lines'); turning 'heralds-minor-mode' off
;;; clears them.
;;;
;;; Code:

(require 'skg-metadata)

(defvar-local skg-fork-dedup-overlay t
  "Non-nil: in this buffer, overlays say what a forks-folder member
restates of its note ('skg-fork-dedup.el').")

(defconst skg-fork-dedup--note-letter "b"
  "The ancestor letter of a forks-folder member's note: its
grandparent, the folder its parent.")

(defun skg-toggle-fork-dedup-overlay ()
  "Toggle `skg-fork-dedup-overlay' in this buffer, and with it the
overlays that cover what a forks-folder member restates of its note."
  (interactive)
  (setq skg-fork-dedup-overlay (not skg-fork-dedup-overlay))
  (skg-fork-dedup-refresh-buffer)
  (message "Restated titles and bodies are %s."
           (if skg-fork-dedup-overlay "covered" "shown")))

(defun skg-fork-dedup-refresh-buffer ()
  "Remove every restatement overlay, then, if `skg-fork-dedup-overlay',
draw those of each forks folder."
  (skg-fork-dedup--clear (point-min) (point-max))
  (when skg-fork-dedup-overlay
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+[ \t]+(skg[ )].*forksFolder" nil t)
        (beginning-of-line)
        (when (skg-fork-dedup--forks-folder-at-point-p)
          (skg-fork-dedup--draw-folder))
        (forward-line 1)))))

(defun skg-fork-dedup-refresh-region (start end)
  "Redraw the restatement overlays an edit of the lines from START to
END can change. Within one headline's lines, those are the forks
folders around that headline (`skg-fork-dedup-refresh-lines'). Lines
holding more than one headline, which a paste or a redraw of the
whole view inserts, can hold any part of any forks folder, so all of
them are redrawn (`skg-fork-dedup-refresh-buffer')."
  (when skg-fork-dedup-overlay
    (if (save-excursion
          (goto-char start)
          (forward-line 1)
          (and (<= (point) end)
               (re-search-forward "^\\*+[ \t]" end t)))
        (skg-fork-dedup-refresh-buffer)
      (skg-fork-dedup-refresh-lines (list start)))))

(defun skg-fork-dedup-refresh-lines (positions)
  "Redraw the restatement overlays an edit at each of POSITIONS can
change: those of the forks folder holding the headline it falls in,
and of the forks folder under that headline, whose note it is."
  (when skg-fork-dedup-overlay
    (let ((folders '()))
      (save-excursion
        (dolist (position positions)
          (goto-char position)
          (when (or (skg--headline-level-on-line)
                    (re-search-backward "^\\*+[ \t]" nil t))
            (beginning-of-line)
            (when (skg-fork-dedup--forks-folder-at-point-p)
              (push (point) folders))
            (let ((parent (car (skg--ancestor-headline-starts-on-line 1))))
              (when (and parent
                         (save-excursion (goto-char parent)
                                         (skg-fork-dedup--forks-folder-at-point-p)))
                (push parent folders)))
            (let ((child (skg-fork-dedup--forks-folder-child)))
              (when child (push child folders)))))
        (dolist (folder (delete-dups folders))
          (goto-char folder)
          (skg-fork-dedup--draw-folder))))))

(defun skg-fork-dedup--clear (start end)
  "Delete the restatement overlays that overlap [START, END)."
  (dolist (ov (overlays-in start end))
    (when (overlay-get ov 'skg-fork-dedup)
      (delete-overlay ov))))

(defun skg-fork-dedup--forks-folder-at-point-p ()
  "Non-nil when the headline on the line at point is a forks folder."
  (eq (skg--folder-atom (skg--metadata-sexp-at-point-or-nil)) 'forksFolder))

(defun skg-fork-dedup--forks-folder-child ()
  "Where the forks folder among the children of the headline at point
begins, or nil."
  (save-excursion
    (let ((level (skg--headline-level-on-line))
          (found nil))
      (when level
        (end-of-line)
        (while (and (not found)
                    (re-search-forward
                     (format "^\\*\\{1,%d\\}[ \t]" (1+ level)) nil t)
                    (> (skg--headline-level-on-line) level))
          (beginning-of-line)
          (if (skg-fork-dedup--forks-folder-at-point-p)
              (setq found (point))
            (end-of-line))))
      found)))

(defun skg-fork-dedup--draw-folder ()
  "Redraw the restatement overlays of the members of the forks folder
whose headline is on the line at point."
  (save-excursion
    (beginning-of-line)
    (let* ((level (skg--headline-level-on-line))
           (note-start (car (skg--ancestor-headline-starts-on-line 1)))
           (note (and note-start
                      (save-excursion (goto-char note-start)
                                      (skg-fork-dedup--entry)))))
      (end-of-line)
      (while (and (re-search-forward
                   (format "^\\*\\{1,%d\\}[ \t]" (1+ level)) nil t)
                  (> (skg--headline-level-on-line) level))
        (beginning-of-line)
        (let ((member (skg-fork-dedup--entry)))
          (skg-fork-dedup--clear (line-beginning-position)
                                 (if member (plist-get member :end)
                                   (line-end-position)))
          (when (and note member)
            (skg-fork-dedup--cover-member member note)))
        (end-of-line)))))

(defun skg-fork-dedup--entry ()
  "For the node headline on the line at point, a plist: :title-start
and :title; :under-start, where the line after the headline begins;
:body, its body with trailing blanks dropped, \"\" when blank;
:body-known, nil when the view omits a body the node has
\('omittedBody'); and :end, where the next headline begins. Nil when
the headline is no node's."
  (let ((metadata (skg--metadata-on-line)))
    (when (and metadata (skg--occurrence-sexp-p (nth 2 metadata)))
      (save-excursion
        (goto-char (nth 1 metadata))
        (skip-chars-forward " \t")
        (let* ((title-start (point))
               (title (buffer-substring-no-properties title-start
                                                      (line-end-position)))
               (under-start (min (point-max) (1+ (line-end-position))))
               (end (progn (goto-char under-start)
                           (if (re-search-forward "^\\*+[ \t]" nil t)
                               (match-beginning 0)
                             (point-max))))
               (body (progn (goto-char end)
                            (skip-chars-backward " \t\n" under-start)
                            (buffer-substring-no-properties
                             under-start (point)))))
          (list :title-start title-start
                :title title
                :under-start under-start
                :body body
                :body-known (not (skg--omitted-body-p (nth 2 metadata)))
                :end end))))))

(defun skg-fork-dedup--cover-member (member note)
  "Draw what MEMBER restates of NOTE, each a `skg-fork-dedup--entry':
with its title and body, \"<b unchanged>\" over its title, and nothing
over the lines under it; with its title alone, \"<b's title
unchanged>\"; with its nonblank body alone, \"<b's body unchanged>\"
over the body. A blank title restates nothing."
  (let* ((title (plist-get member :title))
         (title-start (plist-get member :title-start))
         (body (plist-get member :body))
         (under-start (plist-get member :under-start))
         (same-title (and (not (string-blank-p title))
                          (string= title (plist-get note :title))))
         (same-body (and (plist-get member :body-known)
                         (plist-get note :body-known)
                         (string= body (plist-get note :body)))))
    (cond ((and same-title same-body)
           (skg-fork-dedup--overlay title-start (+ title-start (length title))
                                    (format "<%s unchanged>"
                                            skg-fork-dedup--note-letter))
           (when (< under-start (plist-get member :end))
             (skg-fork-dedup--overlay under-start (plist-get member :end) nil)))
          (same-title
           (skg-fork-dedup--overlay title-start (+ title-start (length title))
                                    (format "<%s's title unchanged>"
                                            skg-fork-dedup--note-letter)))
          ((and same-body (not (string-empty-p body)))
           (skg-fork-dedup--overlay under-start (+ under-start (length body))
                                    (format "<%s's body unchanged>"
                                            skg-fork-dedup--note-letter))))))

(defun skg-fork-dedup--overlay (start end message)
  "A restatement overlay over [START, END) showing MESSAGE in the
message face, or nothing when MESSAGE is nil."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'display
                 (if message (propertize message 'face 'heralds-message-face)
                   ""))
    (overlay-put ov 'skg-fork-dedup t)
    (overlay-put ov 'evaporate t)))

(provide 'skg-fork-dedup)
