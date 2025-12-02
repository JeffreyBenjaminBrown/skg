;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Extract, search for, and visit (in a new view) IDs.

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
