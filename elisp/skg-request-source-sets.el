;;; -*- lexical-binding: t; -*-

(require 'skg-buffer)
(require 'skg-config)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-rerender-all-views) ; for skg--register-rerender-stream-handlers
(require 'skg-state)

(defun skg-list-source-sets ()
  "Ask the server for configured source-sets."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'source-sets
     (lambda (_tcp-proc payload)
       (condition-case err
           (let* ((response (read payload))
                  (active (cadr (assoc 'active response)))
                  (sets (cadr (assoc 'sets response))))
             (message "Active source-set: %s; available: %s"
                      active
                      (mapconcat #'identity sets ", ")))
         (error
          (message "skg-list-source-sets: %S" err))))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "list source sets")))
             "\n"))))

(defun skg-active-source-set ()
  "Ask the server for the active source-set."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'active-source-set
     (lambda (_tcp-proc payload)
       (condition-case err
           (let* ((response (read payload))
                  (content (cadr (assoc 'content response))))
             (message "%s" content))
         (error
          (message "skg-active-source-set: %S" err))))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "active source set")))
             "\n"))))

(defun skg-set-active-source-set (name)
  "Set the active source-set for this TCP connection to NAME.
Open SKG buffers are kept and re-rendered in place: the server
replies with the active-source-set confirmation followed by the
rerender stream (rerender-lock, rerender-view*, rerender-done)."
  (interactive (list (skg--prompt-for-source-set)))
  (when (yes-or-no-p "Switch source-set and re-render all SKG buffers? ")
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'active-source-set
       (lambda (_tcp-proc payload)
         (condition-case err
             (let* ((response (read payload))
                    (content (cadr (assoc 'content response))))
               (message "%s" content))
           (error
            (message "skg-set-active-source-set: %S" err))))
       t)
      (skg--begin-stream "rerender")
      (skg--lock-all-skg-buffers)
      (skg--register-rerender-stream-handlers)
      (skg-lp-reset)
      (process-send-string
       tcp-proc
       (concat (prin1-to-string
                `((request . "set active source set")
                  (name . ,name)))
               "\n")))))

(provide 'skg-request-source-sets)
