;;; -*- lexical-binding: t; -*-

(require 'skg-buffer)
(require 'skg-config)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-rerender-all-views) ; for skg--send-streaming-request
(require 'skg-state)

(defun skg-list-repo-sets ()
  "Ask the server for the skgrepo restriction and its rungs.
Shows each owned repo by its name, the most private one with
\" (no restriction)\" appended, and whether foreign repos are shown."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'repo-sets
     (lambda (_tcp-proc payload)
       (condition-case err
           (let* ((response (read payload))
                  (sets (cadr (assoc 'sets response))))
             (message "%s; available: %s"
                      (skg--skgrepo-restriction-message response)
                      (mapconcat #'skg--skgrepo-restriction-label sets ", ")))
         (error
          (message "skg-list-repo-sets: %S" err))))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "list repo sets")))
             "\n"))))

(defun skg-show-skgrepo-restriction ()
  "Ask the server for the skgrepo restriction."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'skgrepo-restriction
     (lambda (_tcp-proc payload)
       (condition-case err
           (message "%s" (skg--skgrepo-restriction-message (read payload)))
         (error
          (message "skg-show-skgrepo-restriction: %S" err))))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "skgrepo restriction")))
             "\n"))))

(defun skg-restrict-repo-set (choice &optional approved-pids)
  "Set the skgrepo restriction for this TCP connection by CHOICE.
CHOICE is a repo name, the most private owned repo to show, which
keeps foreign repos shown or hidden as they were; or the symbol
`foreign-shown' or `foreign-hidden', which shows or hides the
foreign repos and keeps the rung.  The config's most private owned
repo, with foreign repos shown, means no skgrepo restriction.
Interactively, one prompt offers the rungs and the two foreign
choices (`skg--prompt-for-repo-set').
Open SKG buffers are kept and re-rendered in place: the server
replies with the skgrepo-restriction confirmation followed by the
rerender stream (rerender-lock, rerender-view*, rerender-done).
Refuses while any view has unsaved edits, which the rerender would
replace."
  (interactive (progn (skg--refuse-repo-set-switch-while-views-unsaved)
                      (list (skg--prompt-for-repo-set t))))
  (skg--refuse-repo-set-switch-while-views-unsaved)
  (when (or approved-pids
            (yes-or-no-p "Switch the skgrepo restriction and re-render all SKG buffers? "))
    (skg--send-streaming-request
     "rerender" 'skgrepo-restriction
     (append `((request . "set skgrepo restriction")
               ,(pcase choice
                  ('foreign-shown '(foreign-repos . "shown"))
                  ('foreign-hidden '(foreign-repos . "hidden"))
                  (_ `(name . ,choice))))
             (when approved-pids
               `((approved-overPrivateText-pids ,@approved-pids))))
     (lambda (_tcp-proc payload)
       (condition-case err
           (message "%s" (skg--skgrepo-restriction-message (read payload)))
         (error
          (message "skg-restrict-repo-set: %S" err))))
     nil nil
     (lambda (pids) (skg-restrict-repo-set choice pids)))))

(defun skg--skgrepo-restriction-message (response)
  "What to show for a skgrepo-restriction RESPONSE.
A refusal carries its words in `content'; otherwise the response
names the skgrepo restriction's rung, shown by its label, and says
whether foreign repos are hidden.  With them hidden, no rung is no
restriction, so the rung is shown by its name alone."
  (or (cadr (assoc 'content response))
      (let ((rung (cadr (assoc 'restriction response))))
        (if (equal (cadr (assoc 'foreign-repos response)) "hidden")
            (format "Skgrepo restriction: %s; foreign repos hidden" rung)
          (format "Skgrepo restriction: %s"
                  (skg--skgrepo-restriction-label rung))))))

(defun skg--refuse-repo-set-switch-while-views-unsaved ()
  "Signal `user-error' while any view has unsaved edits."
  (when (skg--unsaved-view-buffers)
    (user-error "Save or revert every skg view before switching the skgrepo restriction")))

(provide 'skg-request-repo-sets)
