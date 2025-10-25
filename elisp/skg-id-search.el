;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Extract, search for, and visit (in a new view) IDs.

;;;###autoload
(defun skg-copy-id-at-point ()
  "RETURNS: This line's ID, or nil.
SIDE EFFECTS: Copies the ID, if found, to the kill-ring."

  (interactive)
  (let* ((line-beg (line-beginning-position))
         (line-end (line-end-position))
         (pos line-beg)
         (found-id nil))
    (while (and (not found-id) (< pos line-end))
      (let ((maybe-id (get-text-property pos 'id)))
        (if maybe-id
            (setq found-id maybe-id)
          (let ((next-change
                 (next-single-property-change ;; PITFALL: If no change is found, this returns pos.
                  pos 'id nil line-end)))
            (setq pos (if (= next-change pos)
                          (1+ line-end) ;; exits `while`
                        next-change))))))
    (if found-id
        (progn
          (kill-new (format "%s" found-id))
          (message "Copied ID: %s" found-id)
          found-id)
      (message "No ID property found on current line"))))

(defun skg-next-line-with-id (id)
  "Search for text with 'id property matching ID starting from point, ignoring the current line (because that's where the user probably just copied the ID from).
.
RETURNS: Nothing.
SIDE EFFECTS: Moves point."

  (interactive "sEnter ID to search for: ")
  (let ((start-pos (point))
        (found nil))
    (setq found (skg-find-id-in-range
                 id start-pos (point-max) nil))
    (unless found
      (setq found (skg-find-id-in-range
                   id (point-min) start-pos t)))
    (unless found
      (message "No text with ID: %s found in buffer"
               id))))

(defun skg-find-id-in-range
    (id start end &optional
        wrapped) ;; wrapped from buffer end to start
  "Search for text with 'id property matching ID from START to END.
.
RETURNS: t (found) or nil (not found).
SIDE EFFECTS: Moves point."

  (let ((current-pos start)
        (found nil))
    (while (and (not found)
                (< current-pos end))
      (let ((next-change (next-single-property-change
                          current-pos 'id nil end)))
        (when (= next-change end)
          (setq current-pos end)) ;; Force exit from loop
        (when (and (< next-change end)
                   (equal (get-text-property
                           next-change 'id)
                          id))
          (setq found t)
          (goto-char next-change)
          (message "Found ID: %s%s" id
                   (if wrapped " (wrapped)" "")))
        (setq current-pos next-change)))
    found))

;;;###autoload
(defun skg-visit-link ()
  "If point is on an org-mode link like [[id:THE_ID][label]],
request a single root content view from that ID.
If point is not on a link, print a message and do nothing."
  (interactive)
  (let* ( ( pos ( point ))
          ( line-start ( line-beginning-position ))
          ( line-end ( line-end-position ))
          ( line-text ( buffer-substring-no-properties
                        line-start line-end ))
          ( link-regex "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" )
          ( found-link nil ) ;; mutates
          ( link-id nil ))   ;; mutates
    (save-excursion
      ;; Search for links in the current line
      (goto-char line-start)
      (while ( and ( not found-link )
                   ( re-search-forward link-regex line-end t ))
        (let ( ( match-start ( match-beginning 0 ))
               ( match-end ( match-end 0 ))
               ( id ( match-string 1 )) )
          (when ( and ;; Check if point is within this link
                  ( >= pos match-start )
                  ( <= pos match-end ))
            (setq found-link t)
            (setq link-id id )) )) )
    (if found-link
        (progn
          (message "Visiting node: %s" link-id)
          (require 'skg-request-single-root-content-view)
          (skg-request-single-root-content-view-from-id link-id))
      (message "Point not on a link")) ))

(provide 'skg-id-search)
