;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Cycle the perspective of the current view: whose
;;; overrides it follows. A fresh view shows each root author's
;;; overrides; the cycle then holds yours and each other owner's whose
;;; overrides would show the view differently, so that you can read it
;;; as that owner would. The server computes the cycle afresh at each
;;; request, switches the view to the next perspective and redraws it;
;;; the answer names the view and the perspective, and a rerender
;;; stream follows, carrying the redrawn buffer, which keeps point and
;;; folds as any redraw the user did not ask for does.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Set overrides
;;; followed".

(require 'skg-buffer)
(require 'skg-request-rerender-all-views) ; for skg--send-streaming-request

(defun skg-override-perspective (&optional approved-pids)
  "Show this view from the next perspective: whose overrides it follows.
A fresh view shows each root author's overrides: under a root of
yours, yours; under another's root, that author's. The cycle then
holds yours and each other owner's whose overrides would show this
view differently, each in every tree, so that each node that owner
overrides shows the override; after the last, the root authors'
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
     "override perspective" 'overrides-followed
     (append `((request . "set overrides followed")
               (view-id . ,skg-view-id)
               (overrides-followed . "next"))
             (when approved-pids
               `((approved-overPrivateText-pids ,@approved-pids))))
     #'skg--override-perspective-handler
     nil nil
     (lambda (pids)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (skg-override-perspective pids)))))))

(defun skg--override-perspective-handler (_tcp-proc payload)
  "Record the perspective the server's answer PAYLOAD names in its
view's buffer, and name it; or show the server's refusal."
  (condition-case err
      (let* ((response (read payload))
             (refusal (cadr (assoc 'content response)))
             (followed (cadr (assoc 'overrides-followed response)))
             (name (cadr (assoc 'perspective-name response)))
             (buffer (skg-find-buffer-by-view-id
                      (cadr (assoc 'view-id response)))))
        (if refusal
            (message "skg: %s" refusal)
          (when buffer
            (with-current-buffer buffer
              (skg--set-override-perspective followed name)
              (force-mode-line-update)))
          (message "Perspective: %s." name)))
    (error
     (message "skg-override-perspective: %S" err))))

(provide 'skg-request-override-perspective)
