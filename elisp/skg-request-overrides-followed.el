;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Switch whose overrides the current view follows: its root
;;; authors' (a fresh view's), or yours, so that every node you
;;; override shows your override, even under a root another wrote. The
;;; server switches the view and redraws it; the answer names the
;;; view and the choice, and a rerender stream follows, carrying the
;;; redrawn buffer, which keeps point and folds as any redraw the user
;;; did not ask for does.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Set overrides
;;; followed".

(require 'skg-buffer)
(require 'skg-request-rerender-all-views) ; for skg--send-streaming-request

(defun skg-switch-overrides-followed (&optional approved-pids)
  "Switch whose overrides this view follows, and redraw it.
A fresh view follows its root authors' overrides: under a root of
yours, yours; under another's root, that author's. Switched, it
follows yours in every tree, so each node you override shows your
override; switched again, it follows its root authors' again. The
mode name reads \"SKG:your overrides\" while it follows yours, and a
save keeps the choice.
Refuses while the buffer has unsaved edits, which the redraw would
replace. APPROVED-PIDS approve the text a privacy challenge asked
about."
  (interactive)
  (unless skg-view-id
    (user-error "Not in a skg view"))
  (when (buffer-modified-p)
    (user-error "Save or revert this buffer first: switching whose overrides it follows redraws it, which would lose its unsaved edits"))
  (let ((buffer (current-buffer)))
    (skg--send-streaming-request
     "overrides followed" 'overrides-followed
     (append `((request . "set overrides followed")
               (view-id . ,skg-view-id)
               (overrides-followed
                . ,(if (equal skg-overrides-followed "user")
                       "root authors"
                     "user")))
             (when approved-pids
               `((approved-overPrivateText-pids ,@approved-pids))))
     #'skg--overrides-followed-handler
     nil nil
     (lambda (pids)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (skg-switch-overrides-followed pids)))))))

(defun skg--overrides-followed-handler (_tcp-proc payload)
  "Record the choice the server's answer PAYLOAD names in its view's
buffer, and say what it now shows; or show the server's refusal."
  (condition-case err
      (let* ((response (read payload))
             (refusal (cadr (assoc 'content response)))
             (followed (cadr (assoc 'overrides-followed response)))
             (buffer (skg-find-buffer-by-view-id
                      (cadr (assoc 'view-id response)))))
        (if refusal
            (message "skg: %s" refusal)
          (when buffer
            (with-current-buffer buffer
              (setq skg-overrides-followed
                    (when (equal followed "user") "user"))
              (force-mode-line-update)))
          (message
           (substitute-command-keys
            (if (equal followed "user")
                "This buffer now shows your overrides wherever you have one. \\[skg-switch-overrides-followed] switches back."
              "This buffer now shows the overrides of each tree's root author, as a fresh view does.")))))
    (error
     (message "skg-switch-overrides-followed: %S" err))))

(provide 'skg-request-overrides-followed)
