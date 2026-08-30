;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "../../elisp" default-directory))

(require 'skg-length-prefix)

(ert-deftest test-skg-lp-binary-chunk-with-nonascii-payload-dispatches ()
  "A binary process chunk is already bytes; encoding it again corrupts UTF-8."
  (let* ((skg-lp--buf (unibyte-string))
         (skg-lp--bytes-left nil)
         (skg--request-records (make-hash-table :test #'equal))
         (skg--request-draft nil)
         (skg--active-request-id "test-request")
         (payload
          (concat
           "((response-type titles-by-ids)"
           " (content ((id . \"Montoya ñó\")))"
           " (request-id test-request) (frame-kind titles-by-ids)"
           " (terminal-status complete))"))
         (payload-bytes
          (encode-coding-string payload 'utf-8))
         (message-bytes
          (concat
           (format "Content-Length: %d\r\n\r\n"
                   (length payload-bytes))
           payload-bytes))
         (seen nil))
    (skg-register-response-handler
     'titles-by-ids
     (lambda (_tcp-proc payload)
       (setq seen payload))
     nil)
    (let ((record skg--request-draft))
      (setf (skg--request-record-id record) "test-request")
      (puthash "test-request" record skg--request-records)
      (setq skg--request-draft nil))
    (skg-lp-handle-generic-chunk nil (substring message-bytes 0 20))
    (skg-lp-handle-generic-chunk nil (substring message-bytes 20))
    (should (equal seen payload))))

(ert-deftest test-skg-request-ids-separate-like-typed-queued-requests ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg--dispatching-request-id nil)
        (skg-lp--pending-count 0)
        sent
        calls)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc wire) (push wire sent))))
      (skg-register-response-handler
       'verify-connection
       (lambda (&rest _) (push 'first calls)) t)
      (let ((first-id
             (skg-submit-request 'proc
                                 "((request . \"verify connection\"))\n")))
        (skg-register-response-handler
         'verify-connection
         (lambda (&rest _) (push 'second calls)) t)
        (let ((second-id
               (skg-submit-request
                'proc "((request . \"verify connection\"))\n")))
          (should (= 1 (length sent)))
          (skg-lp--dispatch-frame
           nil
           (format
            "((response-type verify-connection) (request-id %S) (frame-kind verify-connection) (terminal-status complete))"
            first-id))
          (should (= 2 (length sent)))
          (skg-lp--dispatch-frame
           nil
           (format
            "((response-type verify-connection) (request-id %S) (frame-kind verify-connection) (terminal-status complete))"
            second-id))
          (should (equal (nreverse calls) '(first second)))
          (should (= 0 (hash-table-count skg--request-records)))
          (should-not skg--active-request-id))))))

(ert-deftest test-skg-terminal-handler-error-still-cleans-request ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg-lp--pending-count 0))
    (cl-letf (((symbol-function 'process-send-string) #'ignore))
      (skg-register-response-handler
       'verify-connection (lambda (&rest _) (error "boom")) t)
      (let ((request-id
             (skg-submit-request
              'proc "((request . \"verify connection\"))\n")))
        (skg-lp--dispatch-frame
         nil
         (format
          "((response-type verify-connection) (request-id %S) (frame-kind verify-connection) (terminal-status failed))"
          request-id))
        (should-not (gethash request-id skg--request-records))
        (should-not skg--active-request-id)
        (should (= 0 skg-lp--pending-count))))))
