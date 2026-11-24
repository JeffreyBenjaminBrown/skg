;;; skg-fork-dedup.el --- Cover what a forks-folder member restates  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; A member of a forks folder often says what the folder's note says:
;;; its title, its body, or both, are the note's. Where so, an overlay
;;; covers the member's title with "<b's title, unchanged>" and its
;;; body with "<b's body, unchanged>", b the note's ancestor letter as
;;; counted from the member (its grandparent, past the folder). The
;;; buffer keeps the real text, which a save sends as it is; the
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
  "Non-nil: in this buffer, an overlay covers the title or body of a
forks-folder member that equals its note's ('skg-fork-dedup.el').")

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
draw one over each title and body a forks-folder member restates."
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
                                      (skg-fork-dedup--title-and-body)))))
      (end-of-line)
      (while (and (re-search-forward
                   (format "^\\*\\{1,%d\\}[ \t]" (1+ level)) nil t)
                  (> (skg--headline-level-on-line) level))
        (beginning-of-line)
        (let ((member (skg-fork-dedup--title-and-body)))
          (skg-fork-dedup--clear (line-beginning-position)
                                 (or (nth 4 member) (line-end-position)))
          (when (and note member)
            (skg-fork-dedup--cover (nth 0 member) (nth 1 note)
                                   (nth 1 member) "title")
            (skg-fork-dedup--cover (nth 2 member) (nth 3 note)
                                   (nth 3 member) "body")))
        (end-of-line)))))

(defun skg-fork-dedup--title-and-body ()
  "For the node headline on the line at point: (TITLE-START TITLE
BODY-START BODY ENTRY-END), BODY-START and BODY nil without a nonblank
body, ENTRY-END where the next headline begins; nil when the headline
is no node's. A body ends, trailing blanks dropped, at ENTRY-END."
  (let ((metadata (skg--metadata-on-line)))
    (when (and metadata (skg--occurrence-sexp-p (nth 2 metadata)))
      (save-excursion
        (goto-char (nth 1 metadata))
        (skip-chars-forward " \t")
        (let* ((title-start (point))
               (title (buffer-substring-no-properties title-start
                                                      (line-end-position)))
               (body-start (min (point-max) (1+ (line-end-position))))
               (body-end (progn (goto-char body-start)
                                (if (re-search-forward "^\\*+[ \t]" nil t)
                                    (match-beginning 0)
                                  (point-max))))
               (body (progn (goto-char body-end)
                            (skip-chars-backward " \t\n" body-start)
                            (buffer-substring-no-properties
                             body-start (point)))))
          (list title-start title
                (and (not (string-empty-p body)) body-start)
                (and (not (string-empty-p body)) body)
                body-end))))))

(defun skg-fork-dedup--cover (start text note-text field)
  "Cover TEXT, which begins at START, with \"<b's FIELD, unchanged>\"
when it is nonblank and equals NOTE-TEXT."
  (when (and start text note-text
             (not (string-blank-p text))
             (string= text note-text))
    (let ((ov (make-overlay start (+ start (length text)))))
      (overlay-put ov 'display
                   (propertize (format "<%s's %s, unchanged>"
                                       skg-fork-dedup--note-letter field)
                               'face 'heralds-message-face))
      (overlay-put ov 'skg-fork-dedup t)
      (overlay-put ov 'evaporate t))))

(provide 'skg-fork-dedup)
