;;; -*- lexical-binding: t; -*-

(require 'skg-buffer)
(require 'skg-config)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-rerender-all-views) ; for skg--send-streaming-request
(require 'skg-state)

(defun skg-list-repo-sets ()
  "Ask the server for the skgrepo restriction and its choices.
Shows each repo by its name, the most private one with
\" (no restriction)\" appended."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'repo-sets
     (lambda (_tcp-proc payload)
       (condition-case err
           (let* ((response (read payload))
                  (restriction (cadr (assoc 'restriction response)))
                  (sets (cadr (assoc 'sets response))))
             (message "Skgrepo restriction: %s; available: %s"
                      (skg--skgrepo-restriction-label restriction)
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

(defun skg-restrict-repo-set (name &optional approved-pids)
  "Set the skgrepo restriction for this TCP connection to NAME.
NAME is the most private repo to show; the config's most private
repo means no skgrepo restriction.
Open SKG buffers are kept and re-rendered in place: the server
replies with the skgrepo-restriction confirmation followed by the
rerender stream (rerender-lock, rerender-view*, rerender-done).
Refuses while any view has unsaved edits, which the rerender would
replace."
  (interactive (progn (skg--refuse-repo-set-switch-while-views-unsaved)
                      (list (skg--prompt-for-repo-set))))
  (skg--refuse-repo-set-switch-while-views-unsaved)
  (when (or approved-pids
            (yes-or-no-p "Switch repo-set and re-render all SKG buffers? "))
    (skg--send-streaming-request
     "rerender" 'skgrepo-restriction
     (append `((request . "set skgrepo restriction")
               (name . ,name))
             (when approved-pids
               `((approved-overPrivateText-pids ,@approved-pids))))
     (lambda (_tcp-proc payload)
       (condition-case err
           (message "%s" (skg--skgrepo-restriction-message (read payload)))
         (error
          (message "skg-restrict-repo-set: %S" err))))
     nil nil
     (lambda (pids) (skg-restrict-repo-set name pids)))))

(defun skg--skgrepo-restriction-message (response)
  "What to show for a skgrepo-restriction RESPONSE.
A refusal carries its words in `content'; otherwise the response
names the skgrepo restriction, shown by its label."
  (or (cadr (assoc 'content response))
      (format "Skgrepo restriction: %s"
              (skg--skgrepo-restriction-label
               (cadr (assoc 'restriction response))))))

(defun skg--refuse-repo-set-switch-while-views-unsaved ()
  "Signal `user-error' while any view has unsaved edits."
  (when (skg--unsaved-view-buffers)
    (user-error "Save or revert every skg view before switching the skgrepo-set")))

(provide 'skg-request-repo-sets)
