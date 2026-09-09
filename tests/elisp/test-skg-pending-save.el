;;; test-skg-pending-save.el --- Durable ordinary-save identity tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-pending-save)
(require 'skg-request-save)

(defmacro skg-test-with-pending-save-root (&rest body)
  `(let* ((temporary (make-temp-file "skg-pending-save-test-" t))
          (skg-config-dir (file-name-as-directory temporary))
          (skg--maintenance-archive-folder "archive")
          (config-file (expand-file-name "skgconfig.toml" temporary)))
     (unwind-protect
         (progn
           (write-region "port = 1731\n" nil config-file nil 'silent)
           ,@body)
       (delete-directory temporary t))))

(defun skg-test-pending-save-record ()
  (let* ((operation-id "11111111-2222-4333-8444-555555555555")
         (intent
          (concat "((request . \"save buffer\")"
                  " (operation-id . \"" operation-id "\"))"))
         (content "* α\nbody\n")
         (fingerprint (skg-pending-save-fingerprint intent content))
         (request (concat (substring intent 0 -1)
                          " (request-base-fingerprint . \""
                          fingerprint "\"))\n")))
    (skg-pending-save-prepare
     :operation-id operation-id
     :request-base-fingerprint fingerprint
     :request request :content content :buffer-id "buffer-1")))

(ert-deftest skg-pending-save-fingerprint-excludes-framing-newline ()
  (let ((intent
         "((request . \"save buffer\") (operation-id . \"11111111-2222-4333-8444-555555555555\"))")
        (content "* α\nbody\n"))
    (should
     (equal (skg-pending-save-fingerprint intent content)
            "6a4052d58590c7178c73c389316be38f59c78c4574272a2907f98dac00e363e6"))))

(ert-deftest skg-pending-save-persists-exact-bytes-and-reloads ()
  (skg-test-with-pending-save-root
   (let* ((record (skg-test-pending-save-record))
          (reloaded (car (skg-pending-save-records)))
          (path (skg-pending-save--path
                 (skg-pending-save--root)
                 (skg-pending-save--field record 'operation-id))))
     (should (equal (skg-pending-save--field reloaded 'request)
                    (skg-pending-save--field record 'request)))
     (should (equal (skg-pending-save--field reloaded 'content)
                    "* α\nbody\n"))
     (should (= (logand (file-modes path) #o777) #o600))
     (should (= (logand (file-modes (file-name-directory path)) #o777)
                #o700)))))

(ert-deftest skg-pending-save-retry-keeps-id-and-exact-material ()
  (skg-test-with-pending-save-root
   (let* ((record (skg-test-pending-save-record))
          (operation-id (skg-pending-save--field record 'operation-id))
          (fingerprint
           (skg-pending-save--field record 'request-base-fingerprint))
          (status
           `((operation-id ,operation-id)
             (request-base-fingerprint ,fingerprint)
             (state unknown)))
          (updated
           (skg-pending-save-apply-status record status))
          (material (skg-pending-save-retry-material updated)))
     (should (string-match-p (regexp-quote operation-id) (car material)))
     (should (equal (cadr material) "* α\nbody\n"))
     (should (= (length (skg-pending-save-records)) 1)))))

(ert-deftest skg-pending-save-block-does-not-change-edited-text ()
  (skg-test-with-pending-save-root
   (skg-test-pending-save-record)
   (with-temp-buffer
     (insert "locally edited\n")
     (let ((before (buffer-string)))
       (should-error (skg-pending-save-assert-none-unresolved)
                     :type 'user-error)
       (should (equal before (buffer-string)))
       (should (buffer-modified-p))))))

(ert-deftest skg-pending-save-ack-compacts-only-a-terminal-record ()
  (skg-test-with-pending-save-root
   (let* ((record (skg-test-pending-save-record))
          (operation-id (skg-pending-save--field record 'operation-id))
          (fingerprint
           (skg-pending-save--field record 'request-base-fingerprint))
          (response
           (prin1-to-string
            `((operation-id ,operation-id)
              (request-base-fingerprint ,fingerprint)
              (save-operation-state committed)))))
     (should-error (skg-pending-save-mark-acknowledged record)
                   :type 'skg-pending-save-error)
     (skg-pending-save-mark-terminal record response)
     (let ((compact (skg-pending-save-mark-acknowledged record)))
       (should (eq (skg-pending-save--field compact 'state) 'acknowledged))
       (should-not (assq 'request compact))
       (should-not (assq 'content compact))
       (should-not (skg-pending-save-unresolved-records))))))

(ert-deftest skg-blocked-save-response-preserves-edited-buffer ()
  (skg-test-with-pending-save-root
   (let* ((record (skg-test-pending-save-record))
          (operation-id (skg-pending-save--field record 'operation-id))
          (fingerprint
           (skg-pending-save--field record 'request-base-fingerprint))
          (response
           (prin1-to-string
            `((response-type save-result)
              (content "server replacement")
              (operation-id ,operation-id)
              (request-base-fingerprint ,fingerprint)
              (save-operation-state blocked)
              (reason "authorized effects require recovery")))))
     (with-temp-buffer
       (insert "locally edited")
       (let ((before (buffer-string)))
         (should (eq (skg--persist-save-response record response) 'blocked))
         (should (equal before (buffer-string)))
         (should (buffer-modified-p)))))))

(ert-deftest skg-recovered-committed-status-requires-fresh-view ()
  (skg-test-with-pending-save-root
   (let* ((record (skg-test-pending-save-record))
          (operation-id (skg-pending-save--field record 'operation-id))
          (fingerprint
           (skg-pending-save--field record 'request-base-fingerprint))
          (terminal
           (concat
            "((response-type save-result)"
            " (save-operation-state committed)"
            " (recovered-after-restart true)"
            " (requires-fresh-view true) (content \"\"))"))
          (status
           `((operation-id ,operation-id)
             (request-base-fingerprint ,fingerprint)
             (state committed)
             (terminal-response ,terminal))))
     (with-temp-buffer
       (insert "unsaved newer edit")
       (let ((before (buffer-string))
             (updated (skg-pending-save-apply-status record status)))
         (should (equal before (buffer-string)))
         (should (buffer-modified-p))
         (should (eq (skg-pending-save--field
                      updated 'fresh-view-required)
                     'true)))))))

(ert-deftest skg-save-fingerprint-is-final-request-field ()
  (let* ((operation-id "11111111-2222-4333-8444-555555555555")
         (fingerprint (make-string 64 ?a))
         (request
          (skg--save-request-sexp
           "uri" '(:point-lines-below-focused-headline 0
                   :point-column 0
                   :point-screen-lines-below-window-start 0)
           nil nil nil nil nil operation-id fingerprint)))
    (should (equal (car (last request))
                   `(request-base-fingerprint . ,fingerprint)))
    (should (equal (cdr (assq 'operation-id request)) operation-id))))

(provide 'test-skg-pending-save)
