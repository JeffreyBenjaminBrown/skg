;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Cycle the perspective of the current view: whose list of
;;; skgrepos ranks the mirrors it shows in a note's place. A fresh view
;;; is drawn from each root author's perspective; the cycle then holds
;;; yours and each other owner's whose perspective would show the view
;;; differently, so that you can read it as that owner would. The
;;; server computes the cycle afresh at each request, switches the
;;; view to the next perspective and redraws it; the answer names the
;;; view and the perspective, and a rerender stream follows, carrying
;;; the redrawn buffer, which keeps point and folds as any redraw the
;;; user did not ask for does.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Set
;;; perspective".

(require 'skg-buffer)
(require 'skg-request-rerender-all-views) ; for skg--send-streaming-request

(defun skg-cycle-perspective (&optional approved-pids)
  "Show this view from the next perspective: whose list of skgrepos
ranks the mirrors it shows in a note's place. A fresh view is drawn
from each root author's perspective: under a root of yours, yours;
under another's root, that author's. The cycle then holds yours and
each other owner's whose perspective would show this view
differently, each in every tree, so that each note shows the mirror
that owner's skgrepos rank first; after the last, the root authors'
again. Each switch names the perspective it shows; the mode name
names it too, unless it is the root authors', and a save keeps it.
Refuses while the buffer has unsaved edits, which the redraw would
replace. APPROVED-PIDS approve the text a privacy challenge asked
about."
  (interactive)
  (unless skg-view-id
    (user-error "Not in a skg view"))
  (when (buffer-modified-p)
    (user-error "Save or revert this buffer first: changing its perspective redraws it, which would lose its unsaved edits"))
  (let ((buffer (current-buffer)))
    (skg--send-streaming-request
     "cycle perspective" 'perspective
     (append `((request . "set perspective")
               (view-id . ,skg-view-id)
               (perspective . "next"))
             (when approved-pids
               `((approved-overPrivateText-pids ,@approved-pids))))
     #'skg--cycle-perspective-handler
     nil nil
     (lambda (pids)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (skg-cycle-perspective pids)))))))

(defun skg--cycle-perspective-handler (_tcp-proc payload)
  "Record the perspective the server's answer PAYLOAD names in its
view's buffer, and name it; or show the server's refusal."
  (condition-case err
      (let* ((response (read payload))
             (refusal (cadr (assoc 'content response)))
             (perspective (cadr (assoc 'perspective response)))
             (name (cadr (assoc 'perspective-name response)))
             (buffer (skg-find-buffer-by-view-id
                      (cadr (assoc 'view-id response)))))
        (if refusal
            (message "skg: %s" refusal)
          (when buffer
            (with-current-buffer buffer
              (skg--set-perspective perspective name)
              (force-mode-line-update)))
          (message "Showing %s." name)))
    (error
     (message "skg-cycle-perspective: %S" err))))

(provide 'skg-request-perspective)
