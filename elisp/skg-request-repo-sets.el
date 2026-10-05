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
                  (active (cadr (assoc 'active response)))
                  (sets (cadr (assoc 'sets response))))
             (message "Active repo-set: %s; available: %s"
                      active
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

(defun skg-active-repo-set ()
  "Ask the server for the active repo-set."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'active-repo-set
     (lambda (_tcp-proc payload)
       (condition-case err
           (let* ((response (read payload))
                  (content (cadr (assoc 'content response))))
             (message "%s" content))
         (error
          (message "skg-active-repo-set: %S" err))))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "active repo set")))
             "\n"))))

(defun skg-restrict-repo-set (name &optional approved-pids)
  "Set the active repo-set for this TCP connection to NAME.
Open SKG buffers are kept and re-rendered in place: the server
replies with the active-repo-set confirmation followed by the
rerender stream (rerender-lock, rerender-view*, rerender-done)."
  (interactive (list (skg--prompt-for-repo-set)))
  (when (or approved-pids
            (yes-or-no-p "Switch repo-set and re-render all SKG buffers? "))
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'active-repo-set
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
       'active-repo-set)
      (skg-lp-reset)
      (process-send-string
       tcp-proc
       (concat (prin1-to-string
                (append
                 `((request . "set active repo set")
                   (name . ,name))
                 (when approved-pids
                   `((approved-overPrivateText-pids ,@approved-pids)))))
               "\n")))))

(provide 'skg-request-repo-sets)
