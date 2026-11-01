;;; -*- lexical-binding: t; -*-

(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'skg-state)

(defun skg-view-new-empty ()
  "Open a blank skg content view for creating new nodes.
The view opens when the server answers the current runtime
generation, which it keeps as its `skg-rendered-at': its save is then
refused if it would write a node that changed after it was opened."
  (interactive)
  (skg-request-runtime-generation #'skg-open-empty-content-view))

(defun skg-request-runtime-generation (callback)
  "Ask the server for the current runtime generation, and call
CALLBACK with it, a string, when the answer arrives."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'runtime-generation
     (lambda (_tcp-proc payload)
       (funcall callback
                (format "%s" (cadr (assoc 'content (read payload))))))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc "((request . \"runtime generation\"))\n")))

(provide 'skg-view-new-empty)
