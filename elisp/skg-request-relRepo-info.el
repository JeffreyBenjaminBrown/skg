;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: `skg-set-relRepo' -- request the relRepo
;;; of one relationship edge, informed by the server's
;;; 'relRepo info' endpoint
;;; (BUG-and-fix_make-edge-more-public.org). The buffer-local
;;; helpers it drives live in skg-metadata.el.

(require 'skg-length-prefix)
(require 'skg-metadata)

(defun skg-set-relRepo (&optional recursive)
  "Set the relRepo of the relationship or alias at point.

With a prefix argument RECURSIVE, instead run
`skg-set-relRepo-recursive', which prompts for a
relationship kind and a repo and applies the repo throughout the
subtree at point.

The headline at point represents one edge: `contains' for a content
child, the folder's relation for a writable PartnerFolder member. This
command asks the server for the edge's DEFAULT repo and its CURRENT
repo, then
prompts -- with both tab-completion and S-left/S-right cycling,
like the other repo dialogs -- over the repos at least as
private as the default (more public ones could leak an endpoint's
ID and would be rejected at save), plus a no-override choice. The
minibuffer starts pre-filled with a pending request when one is
offerable, else the current relRepo or default, so RET preserves the
most specific available choice.

Choosing a repo writes an `(editRequest (relRepo REPO))'
metadata request. The no-override choice removes that request, which on save means the edge
keeps its saved repo (sticky), NOT that it resets to its default.
To lower an edge's privacy to its default (e.g. after making the
more private endpoint's home more public), choose the default relRepo
itself; once saved at the default, the display fact and its red ~herald
stop being rendered.

Refuses on write-protected folder members (the edge belongs to the other
end) and on root headlines (no edge). Like other metadata edits,
this only modifies the buffer; it does NOT save. Call
`skg-request-save-buffer' afterward. The server re-validates at
save time, so a stale or hand-typed repo more public than the
edge's default is still rejected there."
  (interactive "P")
  (if recursive
      (skg-set-relRepo-recursive)
    (skg--set-relRepo-at-point)))

(defun skg--set-relRepo-at-point ()
  "The single-edge path of `skg-set-relRepo': classify
the edge at point, ask the server for its (default, current) repos,
and prompt from the reply."
  (let ((buffer (current-buffer))
        (marker (point-marker)))
    (if (skg--alias-headline-p)
        (let ((default
               (save-excursion
                 (unless (and (org-up-heading-safe)
                              (org-up-heading-safe))
                   (user-error "Alias has no owning node headline"))
                 (skg--current-node-repo)))
              (current (skg--relRepo-current-value)))
          (skg--set-relRepo-from-info
           buffer marker
           (format "((response-type relRepo-info) (default %S)%s)"
                   default
                   (if current
                       (format " (current %S)" current)
                     ""))))
      (let ((edge (skg--rel-at-point)))
        (skg-register-response-handler
         'relRepo-info
         (lambda (_tcp-proc payload)
           (skg--set-relRepo-from-info buffer marker payload))
         t)
        (skg-lp-reset)
        (process-send-string
         (skg-tcp-connect-to-rust)
         (concat
          (prin1-to-string
           `((request . "relRepo info")
             (owner . ,(plist-get edge :owner))
             (member . ,(plist-get edge :member))
             (relation . ,(plist-get edge :relation))))
          "\n"))))))

(defun skg--set-relRepo-from-info (buffer marker payload)
  "Handle the relRepo-info response for `skg-set-relRepo'.
Parses PAYLOAD, then prompts and applies the choice at MARKER in
BUFFER. The prompt runs from a zero-delay timer so the minibuffer
opens outside the network process filter."
  (let* ((response (read payload))
         (as-string (lambda (v) (and v (format "%s" v))))
         (err     (funcall as-string (cadr (assoc 'error response))))
         (default (funcall as-string (cadr (assoc 'default response))))
         (current (funcall as-string (cadr (assoc 'current response)))))
    (run-at-time
     0 nil
     (lambda ()
       (if (not (buffer-live-p buffer))
           (message "skg: buffer vanished before the relRepo prompt")
         (with-current-buffer buffer
           (save-excursion
             (goto-char marker)
             (when err
               (message "relRepo info: %s -- offering every repo; the save will validate."
                        err))
             (let* ((ladder (skg--repo-names))
                    (requested (skg--relRepo-requested-value))
                    (choices (append (skg--relRepo-choices
                                      ladder default)
                                     (list skg--relRepo-no-override)))
                    (prompt (concat "relRepo (S-left/right cycle"
                                    (when default
                                      (format "; default %s" default))
                                    (when current
                                      (format "; currently %s" current))
                                    "): "))
                    ;; Pre-fill so RET keeps the status quo and the
                    ;; cycle starts from it. A legacy CURRENT more
                    ;; public than the default is not among the choices;
                    ;; fall back to the default, then to empty.
                    (prefill (cond ((and requested (member requested choices))
                                    requested)
                                   ((and current (member current choices))
                                    current)
                                   ((and default (member default choices))
                                    default)))
                    (choice (skg--completing-read-with-cycle
                             prompt choices nil t prefill nil nil nil
                             choices)))
               (message "%s"
                        (skg--apply-relRepo-choice
                         choice))))))))))

(provide 'skg-request-relRepo-info)
