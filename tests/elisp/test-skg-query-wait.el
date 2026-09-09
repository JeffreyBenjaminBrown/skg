;;; test-skg-query-wait.el --- durable query wait client tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../../elisp" default-directory))
(require 'skg-query-wait)

(ert-deftest test-skg-query-wait-wire-separates-operation-from-request-id ()
  (let* ((skg--active-source-set-name "all")
         (record (list :query-operation-id "query-1"
                       :target '(:incident-id "incident-1"
                                 :maintenance-epoch 7)
                       :recipe '((kind . "text-search") (terms . "apple"))
                       :recipe-digest "digest-1"
                       :destination
                       '((client-buffer-id . "buffer-1")
                         (view-uri . "search:wait:query-1")
                         (client-application-token . 3)
                         (base-content-sha256 . "content-digest"))))
         (wire (prin1-to-string (skg-query-wait--request-fields record))))
    (should (string-match-p "query-operation-id" wire))
    (should (string-match-p "request.*query wait" wire))
    (should (string-match-p "incident-id" wire))
    (should (string-match-p "maintenance-epoch" wire))
    (should (string-match-p "base-content-sha256" wire))
    (should-not (string-match-p "request-id" wire))))

(ert-deftest test-skg-query-wait-applies-result-and-reacks-duplicate ()
  (let* ((operation-id "query-result")
         (session "11111111-2222-4333-8444-555555555555")
         (skg--server-session-id session)
         (content "* result\nexact\n")
         (base "* waiting\n")
         (base-digest (secure-hash 'sha256 base))
         (result-digest (secure-hash 'sha256 content))
         (buffer (generate-new-buffer " *skg-query-result*"))
         (record (make-skg--buffer-record
                  :id "buffer-result" :kind 'search-view
                  :lifecycle 'live-view :disposable nil :buffer buffer
                  :view-uri "search:wait:query-result" :source-set "all"
                  :server-session-id session :view-write-authority 'read-only
                  :graph-generation 0 :presentation-generation 0
                  :server-revision 0 :application-token 1
                  :last-fetched base :last-fetched-sha256 base-digest))
         (skg--query-waits (make-hash-table :test #'equal))
         acks)
    (with-current-buffer buffer
      (insert base)
      (setq-local skg--query-operation-id operation-id
                  skg-view-uri "search:wait:query-result"
                  skg--buffer-record record
                  buffer-read-only t)
      (set-buffer-modified-p nil))
    (puthash operation-id
             (list :query-operation-id operation-id :status 'pending
                   :recipe-digest "recipe" :result-digest nil
                   :freshness nil :reason nil :buffer buffer)
             skg--query-waits)
    (cl-letf (((symbol-function 'skg-submit-request)
               (lambda (&rest args) (push args acks)))
              ((symbol-function 'skg-register-response-handler)
               (lambda (&rest _) nil)))
      (let ((response
             (format
              "((response-type query-wait-result) (query-operation-id %S) (server-session-id %S) (view-write-authority read-only) (view-uri %S) (client-buffer-id %S) (expected-client-application-token 1) (resulting-client-application-token 2) (query-recipe-digest %S) (base-content-sha256 %S) (expected-graph-generation 0) (expected-presentation-generation 0) (expected-server-revision 0) (graph-generation 1) (presentation-generation 1) (server-revision 1) (source-set %S) (freshness current) (result-digest %S) (content %S))"
              operation-id session "search:wait:query-result" "buffer-result"
              "recipe" base-digest "all" result-digest content)))
        (skg-query-wait-result-handler nil response)
        (should (eq (plist-get (gethash operation-id skg--query-waits)
                              :status)
                    'delivered))
        (should (equal content (with-current-buffer buffer (buffer-string))))
        (let ((ack-count (length acks)))
          (skg-query-wait-result-handler nil response)
          (should (> (length acks) ack-count))
          (should (equal content (with-current-buffer buffer (buffer-string)))))))
    (kill-buffer buffer)))

(ert-deftest test-skg-query-wait-rejects-stale-destination-and-digest ()
  (let* ((operation-id "query-reject")
         (session "11111111-2222-4333-8444-555555555555")
         (skg--server-session-id session)
         (base "* waiting\n")
         (base-digest (secure-hash 'sha256 base))
         (buffer (generate-new-buffer " *skg-query-reject*"))
         (record (make-skg--buffer-record
                  :id "buffer-reject" :kind 'search-view
                  :lifecycle 'live-view :disposable nil :buffer buffer
                  :view-uri "search:wait:query-reject" :source-set "all"
                  :server-session-id session :view-write-authority 'read-only
                  :graph-generation 0 :presentation-generation 0
                  :server-revision 0 :application-token 1
                  :last-fetched base :last-fetched-sha256 base-digest))
         (skg--query-waits (make-hash-table :test #'equal)))
    (with-current-buffer buffer
      (insert base)
      (setq-local skg--query-operation-id operation-id
                  skg-view-uri "search:wait:query-reject"
                  skg--buffer-record record
                  buffer-read-only t)
      (set-buffer-modified-p nil))
    (puthash operation-id
             (list :query-operation-id operation-id :status 'pending
                   :recipe-digest "recipe" :result-digest nil
                   :freshness nil :reason nil :buffer buffer)
             skg--query-waits)
    (cl-letf (((symbol-function 'skg-submit-request) (lambda (&rest _) nil))
              ((symbol-function 'skg-register-response-handler)
               (lambda (&rest _) nil)))
      (let ((response
             (format
              "((response-type query-wait-result) (query-operation-id %S) (server-session-id %S) (view-write-authority read-only) (view-uri stale-uri) (client-buffer-id %S) (expected-client-application-token 1) (resulting-client-application-token 2) (query-recipe-digest %S) (base-content-sha256 %S) (expected-graph-generation 0) (expected-presentation-generation 0) (expected-server-revision 0) (graph-generation 1) (presentation-generation 1) (server-revision 1) (source-set %S) (freshness current) (result-digest %S) (content %S))"
              operation-id session "buffer-reject" "recipe" base-digest
              "all" (make-string 64 ?a) "* changed\n")))
        (skg-query-wait-result-handler nil response)
        (should (eq (plist-get (gethash operation-id skg--query-waits)
                              :status)
                    'destination-rejected))
        (should (string-match-p "destination base"
                                (plist-get (gethash operation-id skg--query-waits)
                                           :reason)))
        (let ((digest-response
               (replace-regexp-in-string
                "view-uri stale-uri" "view-uri \"search:wait:query-reject\""
                response t t)))
          (setf (plist-get (gethash operation-id skg--query-waits) :status)
                'pending)
          (skg-query-wait-result-handler nil digest-response)
          (should (equal "Skg query wait destination base or result digest changed"
                         (plist-get (gethash operation-id skg--query-waits)
                                    :reason))))))
    (kill-buffer buffer)))

(ert-deftest test-skg-query-wait-choice-selects-wait-only-on-rebuild ()
  (let ((skg--rebuilding t))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should (equal "wait" (skg-query-wait-policy-choice)))))
  (let ((skg--rebuilding nil)
        (skg--maintenance-client-incident nil)
        (skg--graph-transition-status nil)
        (skg--graph-write-admission nil))
    (should (equal "current" (skg-query-wait-policy-choice)))))

(ert-deftest test-skg-query-wait-choice-covers-other-reconciliation-phases ()
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
    (let ((skg--rebuilding nil)
          (skg--graph-transition-status 'preparing)
          (skg--graph-write-admission 'open))
      (should (equal "wait" (skg-query-wait-policy-choice))))
    (let ((skg--rebuilding nil)
          (skg--graph-transition-status 'idle)
          (skg--graph-write-admission 'closed))
      (should (equal "wait" (skg-query-wait-policy-choice))))))

(ert-deftest test-skg-query-wait-result-requires-authority ()
  (let ((skg--query-waits (make-hash-table :test #'equal))
        (skg--server-session-id "11111111-2222-4333-8444-555555555555"))
    (puthash "query-1" (list :query-operation-id "query-1") skg--query-waits)
    (should-error
     (skg-query-wait-result-handler
      nil
      "((response-type query-wait-result) (query-operation-id query-1) (server-session-id \"11111111-2222-4333-8444-555555555555\"))"))))

(ert-deftest test-skg-query-wait-does-not-target-report-only-incident ()
  (let ((skg--maintenance-client-incident nil)
        (skg--pending-incidents
         '(((incident-id . "old") (maintenance-epoch . 4)
            (phase . active))))
        (skg--pending-maintenance-offer nil))
    (should-not (skg-query-wait--target))))

(ert-deftest test-skg-query-wait-restart-ingests-bufferless-status-and-verifies-recipe ()
  (let* ((skg--query-waits (make-hash-table :test #'equal))
         (skg--server-session-id "11111111-2222-4333-8444-555555555555")
         (recipe-text "((kind . \"text-search\") (terms . \"apple\"))")
         (digest (secure-hash 'sha256 recipe-text))
         wire)
    (skg-update-global-server-status
     '((server-session-id "11111111-2222-4333-8444-555555555555")
       (owner-publication-revision 1)
       (pending-query-waits
        (((query-operation-id "restart-ready") (status ready))
         ((query-operation-id "restart-blocked") (status blocked))))))
    (should (equal 'ready
                   (plist-get (gethash "restart-ready" skg--query-waits)
                              :status)))
    (should-not (plist-get (gethash "restart-ready" skg--query-waits)
                           :buffer))
    (cl-letf (((symbol-function 'skg-submit-request)
               (lambda (_tcp request) (setq wire request)))
              ((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (&rest _) nil)))
      (skg-query-wait-recover "restart-ready")
      (let ((request (read wire)))
        (should (equal "query wait status" (cdr (assq 'request request))))
        (should (equal "restart-ready"
                       (cdr (assq 'query-operation-id request))))
        (should (equal "11111111-2222-4333-8444-555555555555"
                       (cdr (assq 'server-session-id request))))
        (should-not (assq 'view-uri request))
        (should-not (assq 'client-buffer-id request))))
    (skg-query-wait--record-status
     (read (format "((status ready) (query-recipe %S) (query-recipe-digest %S))"
                   recipe-text digest))
     "restart-ready")
    (let ((record (gethash "restart-ready" skg--query-waits)))
      (should (equal recipe-text (plist-get record :recipe-text)))
      (should (equal digest (plist-get record :recipe-digest))))
    (should-error
     (skg-query-wait--record-status
      (read (format "((status ready) (query-recipe %S) (query-recipe-digest %S))"
                    recipe-text (make-string 64 ?a)))
      "restart-blocked")
     :type 'error)
    (should-not (plist-get (gethash "restart-blocked" skg--query-waits)
                           :recipe-text))))

(ert-deftest test-skg-query-wait-unsolicited-blocked-status-is-coarse-and-deduplicated ()
  (let* ((skg--query-waits (make-hash-table :test #'equal))
         (skg--server-session-id "11111111-2222-4333-8444-555555555555")
         (notifications 0)
         (payload
          "((server-session-id \"11111111-2222-4333-8444-555555555555\") (query-operation-id \"push-blocked\") (status blocked) (reason \"waiting for publication\"))"))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq notifications (1+ notifications)))))
      (skg-query-wait--status-push-handler nil payload)
      (skg-query-wait--status-push-handler nil payload))
    (let ((record (gethash "push-blocked" skg--query-waits)))
      (should record)
      (should (eq 'blocked (plist-get record :status)))
      (should-not (plist-get record :buffer)))
    (should (= 1 notifications))))
