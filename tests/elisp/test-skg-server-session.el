;;; test-skg-server-session.el --- Protocol-v2 session authority tests.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-buffer-registry)
(require 'skg-request-save)
(require 'skg-request-verify-connection)

(defconst skg-test-session-old "11111111-2222-4333-8444-555555555555")
(defconst skg-test-session-new "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")

(defun skg-test-handshake-response (session-id &optional protocol-version)
  (prin1-to-string
   `((response-type verify-connection)
     (protocol-version ,(or protocol-version 2))
     (server-session-id ,session-id)
     (source-inventory ()) (active-source-set all)
     (maintenance-archive-folder archive)
     (maintenance-archive-identity /tmp/archive)
     (maintenance-epoch 0) (maintenance-state idle)
     (census-required true) (graph-generation 7)
     (manifest-revision 9) (typedb-health healthy)
     (tantivy-health healthy) (content connected)
     (current-graph-generation 7) (current-manifest-revision 9)
     (owner-publication-revision 1)
     (graph-write-admission open) (graph-transition-status idle)
     (rebuilding nil) (pending-incidents ()))))

(ert-deftest test-skg-protocol-v2-handshake-is-explicit ()
  (let ((request (read (skg--connection-handshake-request))))
    (should (= 2 (cdr (assoc 'protocol-version request))))))

(ert-deftest test-skg-handshake-accepts-unquoted-server-session-uuid ()
  (should
   (equal skg-test-session-new
          (skg--handshake-authority
           (read (concat "((protocol-version 2) (server-session-id "
                         skg-test-session-new "))"))))))

(ert-deftest test-skg-queued-request-is-stamped-after-handshake ()
  (let* ((skg--server-session-id skg-test-session-new)
         (wire "((request . \"text search\") (request-id . \"r1\"))\n")
         (stamped (skg--stamp-queued-request-wire wire)))
    (should (string-match-p
             (regexp-quote
              (concat "(server-session-id . \"" skg-test-session-new "\")"))
             stamped))))

(ert-deftest test-skg-client-constructor-gate-reopens-on-authoritative-publication ()
  (let ((skg--server-session-id skg-test-session-new)
        (skg--graph-write-admission 'open)
        (skg--client-constructor-admission 'closed)
        (skg--server-store-state nil))
    (should (eq 'read-only (skg-requested-view-write-authority)))
    (should-error
     (skg-view-write-authority-from-response
      '((server-session-id "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
        (view-write-authority editable))))
    (skg-update-global-server-status
     '((server-session-id "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
       (owner-publication-revision 1) (current-graph-generation 8)
       (current-manifest-revision 10)
       (graph-write-admission open) (graph-transition-status idle)
       (rebuilding nil) (pending-incidents ())))
    (should (eq 'open skg--client-constructor-admission))
    (should (eq 'editable (skg-requested-view-write-authority)))))

(ert-deftest test-skg-older-owner-publication-cannot-roll-back-global-state ()
  (let ((skg--server-session-id skg-test-session-new)
        (skg--owner-publication-revision nil)
        (skg--client-constructor-admission 'closed)
        (skg--server-store-state nil))
    (skg-update-global-server-status
     '((server-session-id "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
       (owner-publication-revision 2) (current-graph-generation 7)
       (current-manifest-revision 9) (graph-write-admission open)
       (graph-transition-status idle) (rebuilding nil) (pending-incidents ())))
    (skg-update-global-server-status
     '((server-session-id "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
       (owner-publication-revision 1) (current-graph-generation 7)
       (current-manifest-revision 9) (graph-write-admission closed)
       (graph-transition-status transitioning) (rebuilding true)
       (pending-incidents ((incident-id old)))))
    (should (= 2 skg--owner-publication-revision))
    (should (eq 'open skg--graph-write-admission))
    (should-not skg--rebuilding)
    (should (eq 'idle skg--graph-transition-status))
    (should-not skg--pending-incidents)
    (should (eq 'open skg--client-constructor-admission))))

(ert-deftest test-skg-newer-owner-publication-wins-in-same-session ()
  (let ((skg--server-session-id skg-test-session-new)
        (skg--owner-publication-revision nil)
        (skg--server-store-state nil))
    (skg-update-global-server-status
     '((server-session-id "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
       (owner-publication-revision 1) (current-graph-generation 7)
       (current-manifest-revision 9) (graph-write-admission closed)
       (graph-transition-status transitioning) (rebuilding true)
       (pending-incidents ())))
    (skg-update-global-server-status
     '((server-session-id "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
       (owner-publication-revision 2) (current-graph-generation 7)
       (current-manifest-revision 9) (graph-write-admission open)
       (graph-transition-status idle) (rebuilding nil) (pending-incidents ())))
    (should (= 2 skg--owner-publication-revision))
    (should (eq 'open skg--graph-write-admission))
    (should-not skg--rebuilding)))

(ert-deftest test-skg-unversioned-global-status-cannot-downgrade-publication ()
  (let ((skg--server-session-id skg-test-session-new)
        (skg--owner-publication-revision 3)
        (skg--graph-write-admission 'open)
        (skg--graph-transition-status 'idle)
        (skg--rebuilding nil)
        (skg--pending-incidents nil)
        (skg--server-store-state '((graph-generation . 8)
                                   (manifest-revision . 10))))
    (skg-update-global-server-status
     '((server-session-id "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
       (current-graph-generation 2) (current-manifest-revision 4)
       (graph-write-admission closed) (graph-transition-status transitioning)
       (rebuilding true) (pending-incidents ((incident-id stale)))))
    (should (= 3 skg--owner-publication-revision))
    (should (eq 'open skg--graph-write-admission))
    (should (eq 'idle skg--graph-transition-status))
    (should-not skg--rebuilding)
    (should-not skg--pending-incidents)
    (should (= 8 (alist-get 'graph-generation skg--server-store-state)))
    (should (= 10 (alist-get 'manifest-revision skg--server-store-state)))))

(ert-deftest test-skg-protocol-mismatch-preserves-text-and-undo ()
  (let ((buffer (generate-new-buffer " *skg-protocol-mismatch*"))
        (skg--connection-handshake-state 'sent)
        (skg--connection-handshake-error nil)
        (skg--server-session-id nil)
        submitted)
    (unwind-protect
        (with-current-buffer buffer
          (buffer-enable-undo)
          (insert "authored text")
          (let ((undo-before (copy-tree buffer-undo-list)))
            (cl-letf (((symbol-function 'skg-install-source-inventory)
                       (lambda (&rest _) (error "must not install")))
                      ((symbol-function 'skg--submit-buffer-census)
                       (lambda (&rest _) (setq submitted t))))
              (should-error
               (skg--install-connection-verification
                nil (skg-test-handshake-response skg-test-session-new 1))
               :type 'error))
            (should (equal (buffer-string) "authored text"))
            (should (equal buffer-undo-list undo-before))
            (should-not submitted)
            (should-not skg--server-session-id)
            (should (eq skg--connection-handshake-state 'failed))))
      (kill-buffer buffer))))

(ert-deftest test-skg-restart-census-keeps-origin-and-detaches-old-view ()
  (let ((buffer (generate-new-buffer " *skg-old-session*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-session-id skg-test-session-old)
        (skg--owner-publication-revision 5)
        (skg--server-store-state '((graph-generation . 7)))
        (skg--active-source-set-name "all")
        census)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (buffer-enable-undo)
            (insert "old live view\nlocal edit")
            (setq skg-view-uri "view-old")
            (skg-register-buffer
             buffer 'content-view :lifecycle 'live-view :disposable nil
             :view-uri "view-old" :server-session-id skg-test-session-old
             :graph-generation 7 :server-revision 4 :application-token 3)
            (insert " preserved")
            (let ((text-before (buffer-string))
                  (undo-before (copy-tree buffer-undo-list)))
              (cl-letf (((symbol-function 'skg-install-source-inventory)
                         #'ignore)
                        ((symbol-function 'skg--submit-buffer-census)
                         (lambda (&rest _) (setq census (skg-buffer-census))))
                        ((symbol-function 'skg--show-handshake-telescope-warnings)
                         #'ignore)
                        ((symbol-function 'skg--show-abandoned-prearchive-maintenance)
                         #'ignore)
                        ((symbol-function 'message) #'ignore))
                (setq skg--server-session-id nil)
                (skg--install-connection-verification
                 nil (skg-test-handshake-response skg-test-session-new)))
              (should (= 1 skg--owner-publication-revision))
              (should (equal skg-test-session-old
                             (cdr (assoc 'server-session-id (car census)))))
              (skg--handle-buffer-census-response
               nil
               (prin1-to-string
                `((server-session-id ,skg-test-session-new)
                  (text-required-buffer-ids ())
                  (stale-buffer-ids
                   (,(skg--buffer-record-id skg--buffer-record))))))
              (should (equal (buffer-string) text-before))
              (should (equal buffer-undo-list undo-before))
              (should-not skg-view-uri)
              (should (equal skg-test-session-old
                             (skg--buffer-record-server-session-id
                              skg--buffer-record))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-stale-delayed-redraw-cannot-replace-authored-text ()
  (let ((buffer (generate-new-buffer " *skg-stale-reply*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-session-id skg-test-session-new)
        (skg--server-store-state '((graph-generation . 7)))
        (skg--active-source-set-name "all"))
    (unwind-protect
        (with-current-buffer buffer
          (buffer-enable-undo)
          (insert "keep local edit")
          (skg-register-buffer
           buffer 'content-view :lifecycle 'live-view :disposable nil
           :view-uri "view" :server-session-id skg-test-session-old
           :graph-generation 7 :server-revision 4 :application-token 3)
          (insert "!")
          (let ((text-before (buffer-string))
                (undo-before (copy-tree buffer-undo-list)))
            (should-error
             (skg-replace-buffer-with-new-content
              nil "stale server text" nil
              (list :server-session-id skg-test-session-old
                    :application-token 4)))
            (should (equal (buffer-string) text-before))
            (should (equal buffer-undo-list undo-before))))
      (kill-buffer buffer))))

(ert-deftest test-skg-save-intent-binds-origin-session-before-fingerprint ()
  (let* ((record (make-skg--buffer-record
                  :id "buffer" :kind 'content-view
                  :graph-generation 7 :server-revision 4
                  :application-token 3
                  :server-session-id skg-test-session-old))
         (intent (prin1-to-string
                  (skg--save-request-sexp
                   "view" '(:point-lines-below-focused-headline 0
                            :point-column 0
                            :point-screen-lines-below-window-start 0)
                   nil nil nil nil record "operation" "fingerprint"))))
    (should (string-match-p
             (regexp-quote
              (concat "(server-session-id . \"" skg-test-session-old
                      "\") (request-base-fingerprint . \"fingerprint\")"))
             intent))))

(provide 'test-skg-server-session)

(ert-deftest test-skg-handshake-preserves-explicit-empty-frozen-census ()
  (let ((skg--server-session-id nil)
        (skg--maintenance-state nil)
        (skg--owner-publication-revision nil)
        (skg--active-source-set-name nil)
        (skg--server-source-inventory nil)
        (skg--maintenance-archive-folder nil)
        (skg--maintenance-archive-identity nil)
        (skg--server-store-state nil)
        (skg--connection-handshake-state nil)
        (skg--client-constructor-admission 'open)
        (skg--graph-write-admission nil)
        (skg--graph-transition-status nil)
        (skg--pending-incidents nil)
        (skg--rebuilding nil)
        (response (read (skg-test-handshake-response skg-test-session-new))))
    (cl-letf (((symbol-function 'skg--submit-buffer-census) #'ignore)
              ((symbol-function 'skg-maintenance-adopt-handshake-epoch) #'ignore))
      (skg--install-connection-verification
       nil (prin1-to-string
            (append response '((maintenance-incident-id a)
                               (maintenance-census-buffer-ids ()))))))
    (should (assq 'census-buffer-ids skg--maintenance-state))
    (should-not (alist-get 'census-buffer-ids skg--maintenance-state))
    (should (equal 'a (alist-get 'incident-id skg--maintenance-state)))))
