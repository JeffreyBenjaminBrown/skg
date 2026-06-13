;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Ask the server for a shell script that stages every
;;; detected node "move" -- a node whose .skg file vanished from one
;;; source's git repo and appeared in another -- and display it.

(require 'skg-length-prefix)

(defun skg-stage-moves ()
  "Display git instructions staging every detected node move.
A node has moved when its .skg file vanished from exactly one source's
git repo (committed there, now gone from the worktree) and appeared in
exactly one other source. The server returns a shell script; review it,
then run it from the skg data root."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'stage-moves
     #'skg--stage-moves-handler
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     "((request . \"stage moves\"))\n")))

(defun skg--stage-moves-handler (_tcp-proc payload)
  "Display a stage-moves response PAYLOAD in a shell-script buffer."
  (condition-case err
      (let* ((response (read payload))
             (content (or (cadr (assoc 'content response))
                          "# stage moves failed\n# Empty response\n"))
             (errors-list (cadr (assoc 'errors response))))
        (with-current-buffer (get-buffer-create "*skg stage moves*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (sh-mode)
            (set-buffer-modified-p nil)
            (goto-char (point-min)))
          (display-buffer (current-buffer)))
        (message "%s"
                 (if errors-list
                     "Stage-moves completed with errors -- see *skg stage moves*"
                   "Stage-moves: review *skg stage moves*, then run it from the data root")))
    (error
     (message "skg: stage-moves handler error: %S" err))))

(provide 'skg-request-stage-moves)
