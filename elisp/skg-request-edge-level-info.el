;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: `skg-set-relationship-source' -- set the privacy level
;;; of one relationship edge, informed by the server's
;;; 'edge level info' endpoint
;;; (BUG-and-fix_make-edge-more-public.org). The buffer-local
;;; helpers it drives live in skg-metadata.el.

(require 'skg-length-prefix)
(require 'skg-metadata)

(defun skg-set-relationship-source (&optional recursive)
  "Set the privacy level of the relationship edge at point.

With a prefix argument RECURSIVE, instead run
`skg-set-relationship-source-recursive', which prompts for a
relationship kind and a level and applies the level throughout the
subtree at point.

The headline at point represents one edge: `contains' for a content
child, the col's relation for a writable PartnerCol member. This
command asks the server for the edge's DEFAULT level (the more
private of the two endpoints' homes) and its CURRENT level, then
prompts -- with both tab-completion and S-left/S-right cycling,
like the other source dialogs -- over the levels at least as
private as the default (more public ones could leak an endpoint's
ID and would be rejected at save), plus a no-override choice. The
minibuffer starts pre-filled with the current level when it is
offerable, else the default, so RET keeps the status quo.

Choosing a level writes a `(relSource LEVEL)' metadata atom. The
no-override choice removes the atom, which on save means the edge
keeps its saved level (sticky), NOT that it resets to its default.
To lower an edge's privacy to its default (e.g. after making the
more private endpoint's home more public), choose the default level
itself; once saved at the default, the atom and its red ~herald
stop being rendered.

Refuses on read-only col members (the edge belongs to the other
end) and on root headlines (no edge). Like other metadata edits,
this only modifies the buffer; it does NOT save. Call
`skg-request-save-buffer' afterward. The server re-validates at
save time, so a stale or hand-typed level more public than the
edge's default is still rejected there."
  (interactive "P")
  (if recursive
      (skg-set-relationship-source-recursive)
    (skg--set-relationship-source-at-point)))

(defun skg--set-relationship-source-at-point ()
  "The single-edge path of `skg-set-relationship-source': classify
the edge at point, ask the server for its (default, current) levels,
and prompt from the reply."
  (let ((edge (skg--relationship-edge-at-point))
        (buffer (current-buffer))
        (marker (point-marker)))
    (skg-register-response-handler
     'edge-level-info
     (lambda (_tcp-proc payload)
       (skg--set-relationship-source-from-info buffer marker payload))
     t)
    (skg-lp-reset)
    (process-send-string
     (skg-tcp-connect-to-rust)
     (concat
      (prin1-to-string
       `((request . "edge level info")
         (owner . ,(plist-get edge :owner))
         (member . ,(plist-get edge :member))
         (relation . ,(plist-get edge :relation))))
      "\n"))))

(defun skg--set-relationship-source-from-info (buffer marker payload)
  "Handle the edge-level-info response for `skg-set-relationship-source'.
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
           (message "skg: buffer vanished before the relationship-source prompt")
         (with-current-buffer buffer
           (save-excursion
             (goto-char marker)
             (when err
               (message "edge level info: %s -- offering every source; the save will validate."
                        err))
             (let* ((ladder (skg--source-names))
                    (choices (append (skg--relationship-source-choices
                                      ladder default)
                                     (list skg--relationship-source-no-override)))
                    (prompt (concat "Relationship level (S-left/right cycle"
                                    (when default
                                      (format "; default %s" default))
                                    (when current
                                      (format "; currently %s" current))
                                    "): "))
                    ;; Pre-fill so RET keeps the status quo and the
                    ;; cycle starts from it. A below-default CURRENT
                    ;; (the foreign shape) is not among the choices;
                    ;; fall back to the default, then to empty.
                    (prefill (cond ((and current (member current choices))
                                    current)
                                   ((and default (member default choices))
                                    default)))
                    (choice (skg--completing-read-with-cycle
                             prompt choices nil t prefill nil nil nil
                             choices)))
               (message "%s"
                        (skg--apply-relationship-source-choice
                         choice))))))))))

(provide 'skg-request-edge-level-info)
