;;; test-skg-recovery-archive.el --- Portable recovery archive tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-recovery-archive)

(defconst skg-test-recovery-incident-id
  "12345678-1234-4234-8234-123456789abc")

(defun skg-test-recovery--write-private (path text)
  (let ((coding-system-for-write 'utf-8-unix)
        (write-region-inhibit-fsync nil))
    (with-temp-buffer
      (insert text)
      (with-file-modes #o600
        (write-region (point-min) (point-max) path nil 'silent nil 'excl)))))

(defun skg-test-recovery--fixture ()
  (let* ((directory (make-temp-file "skg-recovery-archive-test-" t))
         (owned (expand-file-name "owned" directory))
         (config-path (expand-file-name "skgconfig.toml" directory))
         (buffer (generate-new-buffer "*skg recovery same title*"))
         (base "* Root λ\nbase\n"))
    (set-file-modes directory #o700)
    (with-file-modes #o700 (make-directory owned))
    (skg-test-recovery--write-private
     config-path
     (concat
      "port = 1\n"
      "maintenance_archive_folder = \"archive\"\n\n"
      "[[sources]]\n"
      "name = \"mine\"\n"
      "path = \"owned\"\n"))
    (setq skg-config-dir directory
          skg--maintenance-archive-folder "archive"
          skg--maintenance-archive-identity "/server/mounted/archive")
    (with-current-buffer buffer
      (org-mode)
      (buffer-disable-undo)
      (insert base)
      (buffer-enable-undo)
      (setq buffer-undo-list nil pending-undo-list nil)
      (skg-register-buffer
       buffer 'content-view
       :view-uri "view:archive-fixture"
       :last-fetched base
       :root-ids '("root-a" "root-b")
       :recipe '((kind . "single-root")
                 (requested "root-a" "root-b"))
       :graph-generation 7
       :presentation-generation 3
       :server-revision 11
       :application-token 5)
      (goto-char (point-max))
      (insert "unsaved café 🐙\nlast line\n")
      (set-buffer-modified-p t)
      (skg-lock-buffer-for-maintenance buffer 4))
    (list
     :directory directory
     :archive-root (expand-file-name "archive" directory)
     :buffer buffer
     :offer
     `((incident-id ,skg-test-recovery-incident-id)
       (epoch 4)
       (origin "explicit-partial-reload")
       (started-at-utc "2026-09-04T12:34:56.123456Z")
       (archive-name
        ,(concat "20260904T123456.123456Z_"
                 skg-test-recovery-incident-id))
       (source-set "all")
       (graph-generation 7)
       (manifest-revision 9)))))

(defun skg-test-recovery--cleanup (fixture)
  (when-let ((buffer (plist-get fixture :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer)))
  (when-let ((directory (plist-get fixture :directory)))
    (when (file-directory-p directory)
      (delete-directory directory t)))
  (setq skg-config-dir nil
        skg--maintenance-archive-folder nil
        skg--maintenance-archive-identity nil))

(defun skg-test-recovery--final-bundle (initial-result)
  (let* ((initial
          (skg-recovery--read-exact-sexpr
           (expand-file-name "manifest.initial.sexp"
                             (plist-get initial-result :path))))
         (buffer-record (car (cadr (assoc 'buffers initial))))
         (buffer-id (cadr (assoc 'buffer-id buffer-record)))
         (buffer-key (cadr (assoc 'buffer-key buffer-record)))
         (readme (skg--utf8-unix-bytes "* Modified node niño\n"))
         (raw (unibyte-string 0 255 254 195 40 10))
         (opaque (concat readme raw))
         (node-root "modified-nodes/node-00000000-deadbeefcafe")
         (records
          (list
           `((artifact-key "artifact-00000000")
             (relative-path ,(concat node-root "/README.org"))
             (purpose "node-readme")
             (byte-offset 0)
             (byte-length ,(length readme))
             (sha256 ,(secure-hash 'sha256 readme)))
           `((artifact-key "artifact-00000001")
             (relative-path
              ,(concat node-root "/raw/path-00000000-012345abcdef.after.skg"))
             (purpose "raw-after")
             (byte-offset ,(length readme))
             (byte-length ,(length raw))
             (sha256 ,(secure-hash 'sha256 raw)))))
         (descriptor
          `((artifact-bundle-format-version 1)
            (incident-id ,skg-test-recovery-incident-id)
            (maintenance-epoch 4)
            (candidate-id "abcdefab-1234-4234-8234-abcdefabcdef")
            (g0-graph-generation 7)
            (g0-manifest-revision 9)
            (g1-graph-generation 8)
            (g1-manifest-revision 10)
            (tantivy-generation 12)
            (server-evidence-sha256 ,(make-string 64 ?a))
            (transfer-manifest-sha256 ,(make-string 64 ?b))
            (artifact-bytes-sha256 ,(secure-hash 'sha256 opaque))
            (artifact-count 2)
            (artifact-bytes ,(length opaque))
            (artifacts ,records)))
         (settlements
          (list
           `((buffer-id ,buffer-id)
             (buffer-key ,buffer-key)
             (kind "content-view")
             (view-uri "view:archive-fixture")
             (dirty "true")
             (impacted "true")
             (parse-uncertain "nil")
             (observed-ids ("root-a"))
             (resolved-primary-ids ("root-a"))
             (base-graph-generation 7)
             (base-presentation-generation 3)
             (base-server-revision 11)
             (base-application-token 5)
             (planned-disposition "interrupted")
             (required-ack "retirement-ack")))))
    (list :descriptor descriptor :opaque opaque :settlements settlements
          :raw raw :raw-relative
          (concat node-root "/raw/path-00000000-012345abcdef.after.skg"))))

(ert-deftest test-skg-recovery-canonical-sexpr-matches-portable-fixture ()
  (should
   (equal
    (skg-recovery-canonical-sexpr
     '((archive-format-version 1)
       (name "a\n\"b\\c")
       (empty nil)))
    "((archive-format-version 1) (name \"a\\n\\\"b\\\\c\") (empty ()))")))

(ert-deftest test-skg-recovery-exact-reader-accepts-record-newline-only ()
  (let* ((directory (make-temp-file "skg-machine-record-test-" t))
         (path (expand-file-name "record.sexp" directory)))
    (unwind-protect
        (progn
          (set-file-modes directory #o700)
          (skg-test-recovery--write-private path "((key value))\n")
          (should (equal (skg-recovery--read-exact-sexpr path)
                         '((key value))))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-buffer
              (insert "((key value))\ntrailing")
              (write-region (point-min) (point-max) path nil 'silent)))
          (should-error (skg-recovery--read-exact-sexpr path)
                        :type 'skg-recovery-archive-error))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest test-skg-recovery-final-settlement-binds-application-not-content ()
  (let* ((settlement
          `((buffer-id "buffer") (buffer-key "none")
            (kind "content-view") (view-uri "view") (dirty "nil")
            (impacted "true") (parse-uncertain "nil")
            (observed-ids ()) (resolved-primary-ids ())
            (base-graph-generation 1) (base-presentation-generation 3)
            (base-server-revision 4) (base-application-token 7)
            (planned-disposition "refreshed")
            (required-ack "application-ack")))
         (application
          `((content "private rendered text")
            (content-sha256 ,(make-string 64 ?a))
            (resulting-graph-generation 2)
            (resulting-presentation-generation 8)
            (resulting-server-revision 5)
            (resulting-application-token 8)))
         (normalized
          (car (skg-recovery--normalize-settlements
                (list (append settlement `((application ,application))))
                nil)))
         (identity (cadr (assoc 'application normalized))))
    (should (equal (cadr (assoc 'content-sha256 identity))
                   (make-string 64 ?a)))
    (should-not (assoc 'content identity))
    (should-error
     (skg-recovery--normalize-settlements (list settlement) nil))))

(ert-deftest test-skg-recovery-publishes-private-checksummed-incident ()
  (let* ((fixture (skg-test-recovery--fixture))
         (buffer (plist-get fixture :buffer))
         (before (skg-buffer-raw-text buffer))
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required"))))
    (unwind-protect
        (let* ((result
                (skg-recovery-archive-publish-initial
                 (plist-get fixture :offer)
                 :buffers (list buffer)
                 :client-nonce "0123456789abcdef01234567"))
               (final (plist-get result :path))
               (snapshot-root (expand-file-name "buffer-snapshots" final))
               (buffer-keys
                (directory-files snapshot-root nil
                                 directory-files-no-dot-files-regexp))
               (buffer-directory
                (expand-file-name (car buffer-keys) snapshot-root))
               (manifest-bytes
                (skg-recovery--read-bytes
                 (expand-file-name "manifest.initial.sexp" final)))
               (manifest
                (read (decode-coding-string manifest-bytes 'utf-8-unix t)))
               (marker
                (read
                 (decode-coding-string
                  (skg-recovery--read-bytes
                   (expand-file-name "ARCHIVE-READY" final))
                  'utf-8-unix t))))
          (should (= (logand (file-modes (plist-get fixture :archive-root))
                             #o777)
                     #o700))
          (should (= (logand (file-modes final) #o777) #o700))
          (should (= (length buffer-keys) 1))
          (should (string-match-p "\\`view-1_[0-9a-f]\\{12\\}\\'"
                                  (car buffer-keys)))
          (should
           (equal
            before
            (decode-coding-string
             (skg-recovery--read-bytes
              (expand-file-name "unsaved-changes.org" buffer-directory))
             'utf-8-unix t)))
          (should (equal before (skg-buffer-raw-text buffer)))
          (should
           (equal (plist-get result :manifest-sha256)
                  (secure-hash 'sha256 manifest-bytes)))
          (should (equal (cadr (assoc 'manifest-kind manifest)) "initial"))
          (should (= (cadr (assoc 'g0-graph-generation manifest)) 7))
          (should (= (length (cadr (assoc 'buffers manifest))) 1))
          (should
           (equal (cadr (assoc 'incident-id marker))
                  skg-test-recovery-incident-id))
          (should
           (equal (cadr (assoc 'manifest-sha256 marker))
                  (plist-get result :manifest-sha256)))
          (should (> (plist-get (plist-get result :sizes) :incident-bytes) 0))
          (should (= (plist-get (plist-get result :sizes) :retained-count) 1)))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-refuses-source-overlap ()
  (let ((fixture (skg-test-recovery--fixture)))
    (unwind-protect
        (progn
          (setq skg--maintenance-archive-folder "owned/archive")
          (let ((error-data
                 (should-error (skg-recovery-resolve-archive-root)
                               :type 'skg-recovery-archive-error)))
            (should (string-match-p "overlap"
                                    (error-message-string error-data)))))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-write-failure-leaves-partial-not-ready ()
  (let* ((fixture (skg-test-recovery--fixture))
         (buffer (plist-get fixture :buffer))
         (before (skg-buffer-raw-text buffer))
         (real-writer (symbol-function 'skg-recovery--write-private-file))
         (calls 0)
         (partial
          (expand-file-name
           (concat ".staging/" skg-test-recovery-incident-id
                   ".fedcba9876543210fedcba98.partial")
           (plist-get fixture :archive-root)))
         (final
          (expand-file-name
           (concat "20260904T123456.123456Z_"
                   skg-test-recovery-incident-id)
           (plist-get fixture :archive-root)))
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required"))))
    (unwind-protect
        (cl-letf (((symbol-function 'skg-recovery--write-private-file)
                   (lambda (&rest arguments)
                     (cl-incf calls)
                     (if (= calls 2)
                         (skg-recovery--fail "simulated disk full")
                       (apply real-writer arguments)))))
          (let ((error-data
                 (should-error
                  (skg-recovery-archive-publish-initial
                   (plist-get fixture :offer)
                   :buffers (list buffer)
                   :client-nonce "fedcba9876543210fedcba98")
                  :type 'skg-recovery-archive-error)))
            (should (string-match-p
                     "simulated disk full"
                     (error-message-string error-data))))
          (should (file-directory-p partial))
          (should-not (file-exists-p final))
          (should (equal before (skg-buffer-raw-text buffer))))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-finalizes-opaque-evidence-replay-safely ()
  (let* ((fixture (skg-test-recovery--fixture))
         (buffer (plist-get fixture :buffer))
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required"))))
    (unwind-protect
        (let* ((initial
                (skg-recovery-archive-publish-initial
                 (plist-get fixture :offer)
                 :buffers (list buffer)
                 :client-nonce "0123456789abcdef01234567"))
               (bundle (skg-test-recovery--final-bundle initial))
               (result
                (skg-recovery-archive-finalize
                 initial (plist-get bundle :descriptor)
                 (plist-get bundle :opaque)
                 (plist-get bundle :settlements)))
               (replayed
                (skg-recovery-archive-finalize
                 initial (plist-get bundle :descriptor)
                 (plist-get bundle :opaque)
                 (plist-get bundle :settlements)))
               (root (plist-get initial :path))
               (final-bytes (skg-recovery--read-bytes
                             (expand-file-name "manifest.final.sexp" root)))
               (final (skg-recovery--read-exact-sexpr
                       (expand-file-name "manifest.final.sexp" root))))
          (should (equal (plist-get result :manifest-sha256)
                         (secure-hash 'sha256 final-bytes)))
          (should (equal (plist-get replayed :manifest-sha256)
                         (plist-get result :manifest-sha256)))
          (should (equal (plist-get bundle :raw)
                         (skg-recovery--read-bytes
                          (expand-file-name
                           (plist-get bundle :raw-relative) root))))
          (should (equal (cadr (assoc 'manifest-kind final)) "final"))
          (should (= (length (cadr (assoc 'node-artifacts final))) 2))
          (should (file-exists-p (expand-file-name "FINALIZED" root)))
          (should (string-match-p
                   "buffer-snapshots/.*/unsaved-changes.org"
                   (decode-coding-string
                    (skg-recovery--read-bytes
                     (expand-file-name
                      "interrupted-buffers/README.org" root))
                    'utf-8-unix t))))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-inspects-ready-and-finalized-archives ()
  (let* ((fixture (skg-test-recovery--fixture))
         (buffer (plist-get fixture :buffer))
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required"))))
    (unwind-protect
        (let* ((initial-result
                (skg-recovery-archive-publish-initial
                 (plist-get fixture :offer)
                 :buffers (list buffer)
                 :client-nonce "0123456789abcdef01234567"))
               (root (plist-get initial-result :path))
               (ready (skg-recovery-archive-inspect root))
               (bundle (skg-test-recovery--final-bundle initial-result)))
          (should (eq (plist-get ready :status) 'archive-ready))
          (should (equal (plist-get ready :origin)
                         "explicit-partial-reload"))
          (should (= (plist-get ready :g0) 7))
          (should-not (plist-get ready :g1))
          (should (= (plist-get ready :dirty-buffers) 1))
          (should (= (plist-get ready :changed-nodes) 0))
          (should (plist-get ready :native-undo-compatible))
          (skg-recovery-archive-finalize
           initial-result (plist-get bundle :descriptor)
           (plist-get bundle :opaque) (plist-get bundle :settlements))
          (let ((final (skg-recovery-archive-inspect root)))
            (should (eq (plist-get final :status) 'finalized))
            (should (= (plist-get final :g1) 8))
            (should (= (plist-get final :changed-nodes) 1))
            (should (= (plist-get final :interrupted-buffers) 1))
            (should (= (plist-get final :released-buffers) 0))
            (should (> (plist-get final :bytes) 0))))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-list-retains-invalid-strict-named-siblings ()
  (let* ((fixture (skg-test-recovery--fixture))
         (buffer (plist-get fixture :buffer))
         (invalid-name
          "20260904T123457.123456Z_abcdefab-1234-4234-8234-abcdefabcdef")
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required"))))
    (unwind-protect
        (let* ((initial-result
                (skg-recovery-archive-publish-initial
                 (plist-get fixture :offer)
                 :buffers (list buffer)
                 :client-nonce "0123456789abcdef01234567"))
               (invalid (expand-file-name
                         invalid-name (plist-get fixture :archive-root))))
          (with-file-modes #o700 (make-directory invalid))
          (let ((summaries (skg-recovery-archive-list)))
            (should (= (length summaries) 2))
            (should
             (eq (plist-get
                  (cl-find invalid-name summaries :test #'equal
                           :key (lambda (entry) (plist-get entry :name)))
                  :status)
                 'invalid))
            (should
             (eq (plist-get
                  (cl-find (file-name-nondirectory
                            (plist-get initial-result :path))
                           summaries :test #'equal
                           :key (lambda (entry) (plist-get entry :name)))
                  :status)
                 'archive-ready))))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-inspection-rejects-corrupt-final-marker ()
  (let* ((fixture (skg-test-recovery--fixture))
         (buffer (plist-get fixture :buffer))
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required"))))
    (unwind-protect
        (let* ((initial-result
                (skg-recovery-archive-publish-initial
                 (plist-get fixture :offer)
                 :buffers (list buffer)
                 :client-nonce "0123456789abcdef01234567"))
               (bundle (skg-test-recovery--final-bundle initial-result))
               (root (plist-get initial-result :path))
               (marker (expand-file-name "FINALIZED" root)))
          (skg-recovery-archive-finalize
           initial-result (plist-get bundle :descriptor)
           (plist-get bundle :opaque) (plist-get bundle :settlements))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-buffer
              (insert "((archive-format-version 1))\n")
              (write-region (point-min) (point-max) marker nil 'silent)))
          (should-error (skg-recovery-archive-inspect root)
                        :type 'skg-recovery-archive-error))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-refuses-changed-opaque-final-evidence ()
  (let* ((fixture (skg-test-recovery--fixture))
         (buffer (plist-get fixture :buffer))
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required"))))
    (unwind-protect
        (let* ((initial
                (skg-recovery-archive-publish-initial
                 (plist-get fixture :offer)
                 :buffers (list buffer)
                 :client-nonce "fedcba9876543210fedcba98"))
               (bundle (skg-test-recovery--final-bundle initial))
               (changed (concat (substring (plist-get bundle :opaque) 0 -1)
                                (unibyte-string 99))))
          (should-error
           (skg-recovery-archive-finalize
            initial (plist-get bundle :descriptor) changed
            (plist-get bundle :settlements))
           :type 'skg-recovery-archive-error)
          (should-not (file-exists-p
                       (expand-file-name "FINALIZED"
                                         (plist-get initial :path)))))
      (skg-test-recovery--cleanup fixture))))

(provide 'test-skg-recovery-archive)
