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

(ert-deftest test-skg-lp-artifact-frame-keeps-tail-opaque-across-splits ()
  (let* ((skg-lp--buf (unibyte-string))
         (skg-lp--bytes-left nil)
         (skg-lp--pending-count 0)
         (skg--request-records (make-hash-table :test #'equal))
         (skg--request-draft nil)
         (skg--active-request-id "artifact-request")
         (descriptor
          (concat
           "((response-type maintenance-evidence) (note \"niño\")"
           " (request-id artifact-request) (frame-kind maintenance-evidence)"
           " (terminal-status complete))"))
         (descriptor-bytes (encode-coding-string descriptor 'utf-8-unix))
         (artifact-bytes (unibyte-string 0 255 254 195 40 10))
         (body (concat descriptor-bytes artifact-bytes))
         (wire
          (concat
           (format
            (concat "Content-Length: %d\r\n"
                    "Content-Type: application/x-skg-artifact-bundle\r\n"
                    "Descriptor-Length: %d\r\n\r\n")
            (length body) (length descriptor-bytes))
           body))
         seen-descriptor
         seen-artifacts)
    (skg-register-response-handler
     'maintenance-evidence
     (lambda (_tcp-proc payload opaque)
       (setq seen-descriptor payload
             seen-artifacts opaque))
     t)
    (let ((record skg--request-draft))
      (setf (skg--request-record-id record) "artifact-request")
      (puthash "artifact-request" record skg--request-records)
      (setq skg--request-draft nil))
    (skg-lp-handle-generic-chunk nil (substring wire 0 31))
    (skg-lp-handle-generic-chunk
     nil (substring wire 31 (- (length wire) 3)))
    (should-not seen-descriptor)
    (skg-lp-handle-generic-chunk nil (substring wire (- (length wire) 3)))
    (should (equal descriptor seen-descriptor))
    (should (equal artifact-bytes seen-artifacts))
    (should-not (multibyte-string-p seen-artifacts))
    (should (= 0 (hash-table-count skg--request-records)))))

(ert-deftest test-skg-lp-artifact-header-refuses-an-impossible-boundary ()
  (should
   (equal
    (skg-lp-step
     (concat "Content-Length: 4\r\n"
             "Content-Type: application/x-skg-artifact-bundle\r\n"
             "Descriptor-Length: 5\r\n\r\nbody")
     nil)
    '(:error "Malformed artifact-bundle header"))))

(ert-deftest test-skg-request-ids-separate-like-typed-queued-requests ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg--dispatching-request-id nil)
        (skg--connection-handshake-state 'verified)
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

(ert-deftest test-skg-authoritative-frame-updates-explicit-rebuilding-status ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg--connection-handshake-state 'verified)
        (skg-lp--pending-count 0)
        (skg--rebuilding nil))
    (cl-letf (((symbol-function 'process-send-string) #'ignore))
      (skg-register-response-handler 'maintenance-status #'ignore t)
      (let ((request-id
             (skg-submit-request
              'proc "((request . \"maintenance status\"))\n")))
        (skg-lp--dispatch-frame
         nil
         (format
          "((response-type maintenance-status) (rebuilding true) (request-id %S) (frame-kind maintenance-status) (terminal-status complete))"
          request-id))
        (should skg--rebuilding)))))

(ert-deftest test-skg-terminal-handler-error-still-cleans-request ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg--connection-handshake-state 'verified)
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

(ert-deftest test-skg-ordinary-request-waits-for-connection-census ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg--dispatching-request-id nil)
        (skg--connection-handshake-state 'census)
        (skg-lp--pending-count 0)
        sent)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc wire) (push wire sent))))
      (let ((request-id
             (skg-submit-request
              'proc "((request . \"text search\"))\n")))
        (should-not sent)
        (should-not skg--active-request-id)
        (should (equal request-id (caar skg--request-queue)))
        (setq skg--connection-handshake-state 'verified)
        (skg--dispatch-next-request)
        (should (= 1 (length sent)))
        (should (equal request-id skg--active-request-id))))))

(ert-deftest test-skg-priority-census-passes-a-waiting-ordinary-request ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg--dispatching-request-id nil)
        (skg--connection-handshake-state 'census)
        (skg-lp--pending-count 0)
        sent)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc wire) (push wire sent))))
      (let ((ordinary-id
             (skg-submit-request
              'proc "((request . \"text search\"))\n"))
            (census-id
             (skg-submit-priority-request
              'proc "((request . \"client census\"))\n" nil)))
        (should (= 1 (length sent)))
        (should (string-match-p "client census" (car sent)))
        (should (equal census-id skg--active-request-id))
        (should (equal ordinary-id (caar skg--request-queue)))))))

(ert-deftest test-skg-handshake-handler-failure-cannot-leak-ordinary-work ()
  (let ((skg--request-records (make-hash-table :test #'equal))
        (skg--request-draft nil)
        (skg--request-queue nil)
        (skg--active-request-id nil)
        (skg--dispatching-request-id nil)
        (skg--connection-handshake-state 'sent)
        (skg-lp--pending-count 0)
        sent
        failed)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc wire) (push wire sent))))
      (let ((handshake-id
             (skg-submit-priority-request
              'proc "((request . \"verify connection\"))\n"
              `((verify-connection
                 ,(lambda (&rest _)
                    (setq skg--connection-handshake-state 'census)
                    (error "could not construct census"))
                 . t)))))
        (skg-set-request-failure-handler
         (lambda (reason) (setq failed reason)))
        (skg-submit-request 'proc "((request . \"text search\"))\n")
        (should (= 1 (length sent)))
        (skg-lp--dispatch-frame
         nil
         (format
          "((response-type verify-connection) (request-id %S) (frame-kind verify-connection) (terminal-status complete))"
          handshake-id))
        (should (= 1 (length sent)))
        (should (string-match-p "could not construct census" failed))
        (should (eq skg--connection-handshake-state 'failed))
        (should (string-match-p
                 "could not construct census"
                 skg--connection-handshake-error))
        (should-not skg--request-queue)
        (should-not skg--active-request-id)))))

(ert-deftest test-skg-server-push-dispatches-without-request-id ()
  (let ((skg--server-push-handlers (make-hash-table :test #'equal))
        seen)
    (skg-register-server-push-handler
     'collateral-view
     (lambda (_tcp-proc payload) (setq seen payload)))
    (skg-lp--dispatch-frame
     nil
     "((response-type collateral-view) (frame-kind collateral-view) (server-push true) (operation-id background-1) (content fresh))")
    (should seen)
    (should (string-match-p "background-1" seen))))
