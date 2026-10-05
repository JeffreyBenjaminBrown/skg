;;; -*- lexical-binding: t; -*-

(require 'skg-buffer)
(require 'skg-config)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-rerender-all-views) ; for skg--register-rerender-stream-handlers
(require 'skg-state)

(defun skg-list-repo-sets ()
  "Ask the server for configured repo-sets."
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
                      restriction
                      (mapconcat #'identity sets ", ")))
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
           (let* ((response (read payload))
                  (content (cadr (assoc 'content response))))
             (message "%s" content))
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
Open SKG buffers are kept and re-rendered in place: the server
replies with the skgrepo-restriction confirmation followed by the
rerender stream (rerender-lock, rerender-view*, rerender-done)."
  (interactive (list (skg--prompt-for-repo-set)))
  (when (or approved-pids
            (yes-or-no-p "Switch repo-set and re-render all SKG buffers? "))
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'skgrepo-restriction
       (lambda (_tcp-proc payload)
         (condition-case err
             (let* ((response (read payload))
                    (content (cadr (assoc 'content response))))
               (message "%s" content))
           (error
            (message "skg-restrict-repo-set: %S" err))))
       t)
      (skg--begin-stream "rerender")
      (skg--lock-all-skg-buffers)
      (skg--register-rerender-stream-handlers)
      (skg--register-rerender-overPrivateText-confirmation
       (lambda (pids) (skg-restrict-repo-set name pids))
       'skgrepo-restriction)
      (skg-lp-reset)
      (process-send-string
       tcp-proc
       (concat (prin1-to-string
                (append
                 `((request . "set skgrepo restriction")
                   (name . ,name))
                 (when approved-pids
                   `((approved-overPrivateText-pids ,@approved-pids)))))
               "\n")))))

(provide 'skg-request-repo-sets)
