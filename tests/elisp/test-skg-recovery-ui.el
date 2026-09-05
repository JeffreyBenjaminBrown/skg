;;; test-skg-recovery-ui.el --- Detached recovery UI tests -*- lexical-binding: t; -*-

(require 'test-skg-recovery-archive)
(require 'skg-recovery-ui)

(defun skg-test-recovery-ui--finalize (fixture)
  (let* ((buffer (plist-get fixture :buffer))
         (skg-recovery-archive-native-undo-function
          (lambda (&rest _)
            '(:status empty :kind undo-fu-session :version "not-required")))
         (initial
          (skg-recovery-archive-publish-initial
           (plist-get fixture :offer)
           :buffers (list buffer)
           :client-nonce "0123456789abcdef01234567"))
         (bundle (skg-test-recovery--final-bundle initial)))
    (skg-recovery-archive-finalize
     initial (plist-get bundle :descriptor) (plist-get bundle :opaque)
     (plist-get bundle :settlements))
    (plist-put fixture :initial-result initial)
    (plist-put fixture :summary
               (skg-recovery-archive-inspect (plist-get initial :path)))
    (plist-put
     fixture :buffer-key
     (skg-recovery--required-text
      (car (skg-recovery--required-list
            (plist-get (plist-get fixture :summary) :initial)
            'buffers "initial manifest"))
      'buffer-key "initial buffer"))
    fixture))

(defun skg-test-recovery-ui--kill (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(ert-deftest test-skg-recovery-ui-opens-independent-authority-free-copies ()
  (let* ((fixture (skg-test-recovery--fixture))
         (text (skg-buffer-raw-text (plist-get fixture :buffer)))
         first second)
    (unwind-protect
        (save-window-excursion
          (skg-test-recovery-ui--finalize fixture)
          (setq first
                (skg-open-interrupted-view
                 (plist-get fixture :summary)
                 (plist-get fixture :buffer-key))
                second
                (skg-open-interrupted-view
                 (plist-get fixture :summary)
                 (plist-get fixture :buffer-key)))
          (should (buffer-live-p first))
          (should (buffer-live-p second))
          (should-not (eq first second))
          (should-not (equal (buffer-name first) (buffer-name second)))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (should (derived-mode-p 'skg-recovery-mode))
              (should (equal text (buffer-string)))
              (should-not buffer-file-name)
              (should-not (local-variable-p 'skg-view-uri))
              (should-not skg--buffer-record)
              (should-not (memq buffer (skg-registered-buffers)))
              (should (eq skg-recovery-native-undo-status
                          'text-only-no-native-history))
              (should-error (skg-recovery-refuse-write)
                            :type 'user-error)))
          (with-current-buffer first (goto-char (point-max)) (insert "edit"))
          (with-current-buffer second
            (should (equal text (buffer-string)))))
      (skg-test-recovery-ui--kill first)
      (skg-test-recovery-ui--kill second)
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-ui-refuses-corrupt-required-text ()
  (let ((fixture (skg-test-recovery--fixture)))
    (unwind-protect
        (save-window-excursion
          (skg-test-recovery-ui--finalize fixture)
          (let* ((summary (plist-get fixture :summary))
                 (record (car (skg-recovery--required-list
                               (plist-get summary :initial) 'buffers
                               "initial manifest")))
                 (current-record
                  (cl-find-if
                   (lambda (artifact)
                     (string-suffix-p
                      "/unsaved-changes.org"
                      (skg-recovery--required-text
                       artifact 'path "buffer artifact")))
                   (skg-recovery--required-list
                    record 'artifacts "initial buffer")))
                 (path (expand-file-name
                        (skg-recovery--required-text
                         current-record 'path "buffer artifact")
                        (plist-get summary :path))))
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-buffer
                (insert "corrupt\n")
                (write-region (point-min) (point-max) path nil 'silent)))
            (should-error
             (skg-open-interrupted-view
              summary (plist-get fixture :buffer-key))
             :type 'skg-recovery-archive-error)))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-ui-refuses-an-explicit-unknown-buffer-key ()
  (let ((fixture (skg-test-recovery--fixture))
        (picker-called nil))
    (unwind-protect
        (progn
          (skg-test-recovery-ui--finalize fixture)
          (let ((skg-recovery-ui-buffer-picker
                 (lambda (&rest _) (setq picker-called t))))
            (should-error
             (skg-open-interrupted-view
              (plist-get fixture :summary) "unknown-buffer")
             :type 'user-error))
          (should-not picker-called))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-ui-lists-and-deletes-finalized-incidents ()
  (let ((fixture (skg-test-recovery--fixture)) list-buffer)
    (unwind-protect
        (save-window-excursion
          (skg-test-recovery-ui--finalize fixture)
          (setq list-buffer (skg-list-maintenance-incidents))
          (with-current-buffer list-buffer
            (should (derived-mode-p 'skg-recovery-list-mode))
            (should (= (length tabulated-list-entries) 1)))
          (let ((path (plist-get (plist-get fixture :summary) :path)))
            (should
             (skg-delete-maintenance-incident
              (plist-get fixture :summary) t))
            (should-not (file-exists-p path))))
      (skg-test-recovery-ui--kill list-buffer)
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-ui-refuses-active-incident-deletion ()
  (let ((fixture (skg-test-recovery--fixture))
        (skg--maintenance-client-incident nil))
    (unwind-protect
        (progn
          (skg-test-recovery-ui--finalize fixture)
          (setq skg--maintenance-client-incident
                (list :incident-id skg-test-recovery-incident-id))
          (should-error
           (skg-delete-maintenance-incident
            (plist-get fixture :summary) t)
           :type 'user-error))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-ui-opens-recorded-root-as-fresh-live-view ()
  (let ((fixture (skg-test-recovery--fixture)) selected-root requested-root)
    (unwind-protect
        (progn
          (skg-test-recovery-ui--finalize fixture)
          (cl-letf (((symbol-function
                      'skg-request-single-root-content-view-from-id)
                     (lambda (root &rest _)
                       (setq requested-root root)))
                    (skg-recovery-ui-root-picker
                     (lambda (roots _prompt)
                       (setq selected-root roots)
                       "root-b")))
            (skg-open-fresh-view-for-interrupted
             (plist-get fixture :summary)
             (plist-get fixture :buffer-key)))
          (should (equal selected-root '("root-a" "root-b")))
          (should (equal requested-root "root-b")))
      (skg-test-recovery--cleanup fixture))))

(ert-deftest test-skg-recovery-ui-reruns-search-only-after-confirmation ()
  (let ((fixture (skg-test-recovery--fixture)) request)
    (unwind-protect
        (progn
          (with-current-buffer (plist-get fixture :buffer)
            (setf (skg--buffer-record-kind skg--buffer-record) 'search-view
                  (skg--buffer-record-recipe skg--buffer-record)
                  '((kind . "search") (terms . "octopus")
                    (regex . t) (body) (operators . t))))
          (skg-test-recovery-ui--finalize fixture)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'skg--request-text-search)
                     (lambda (&rest arguments) (setq request arguments))))
            (skg-open-fresh-view-for-interrupted
             (plist-get fixture :summary)
             (plist-get fixture :buffer-key)))
          (should (equal request '("octopus" t nil t nil))))
      (skg-test-recovery--cleanup fixture))))

(provide 'test-skg-recovery-ui)
