;;; test-skg-receipt.el --- Read receipts -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-request-save)
(require 'skg-request-receipt)
(require 'skg-keymaps-and-aliases)

(defun test--receipt-run (body)
  "Run BODY with the network faked. BODY receives a function giving
the sent requests (newest first) and the handlers."
  (let ((sent nil) (handlers (make-hash-table :test 'eq))
        (skg--rerender-after-empty-stream nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg--begin-stream) #'ignore)
              ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
              ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
              ((symbol-function 'skg--wrap-rerender-done-with-after-empty-stream) #'ignore)
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers))))
      (funcall body (lambda () sent) handlers))))

(defmacro test--in-mailbox-view (&rest body)
  "Run BODY in a clean view of a mailbox, point in an item's body."
  (declare (indent 0))
  `(with-temp-buffer
     (skg-content-view-mode)
     (setq-local skg-view-id "mailbox")
     (insert "* (skg node (gnid t) (skgRepos (nodeRepo sender))) to reader\n"
             "** (skg node (gnid m1) (skgRepos (nodeRepo sender))) message one\n"
             "Its body.\n")
     (set-buffer-modified-p nil)
     (goto-char (point-max))
     (forward-line -1)
     ,@body))

(defun test--receipt-message (thunk)
  "What THUNK shows in the echo area."
  (let ((shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq shown (apply #'format format-string args)))))
      (funcall thunk))
    shown))

(ert-deftest test-receipt-refuses-a-buffer-with-unsaved-edits ()
  (test--in-mailbox-view
    (set-buffer-modified-p t)
    (test--receipt-run
     (lambda (sent _handlers)
       (let ((err (should-error (skg-receipt) :type 'user-error)))
         (should (string-match-p "unsaved edits" (cadr err))))
       (should (null (funcall sent)))))))

(ert-deftest test-receipt-sends-the-item-at-point-and-the-text-typed ()
  "From the item's body, the item is its headline's node; the text
read defaults to \"Read.\"."
  (test--in-mailbox-view
    (test--receipt-run
     (lambda (sent handlers)
       (cl-letf (((symbol-function 'read-string)
                  (lambda (_prompt _initial _history default) default)))
         (skg-receipt))
       (should (string-match-p
                (regexp-quote "(request . \"receipt\") (item . \"m1\") (text . \"Read.\")")
                (car (funcall sent))))
       (should (gethash 'receipt handlers))))))

(ert-deftest test-receipt-shows-what-it-wrote ()
  (should (equal (test--receipt-message
                  (lambda ()
                    (skg--receipt-handler
                     "m1" "Read." nil nil
                     "((response-type receipt) (preview false) (lines (\"Forks 'message one'.\" \"Hides 'message one' from 'f'.\")) (receipt-id r) (holder-id h))")))
                 "Forks 'message one'.\nHides 'message one' from 'f'.")))

(ert-deftest test-receipt-asks-for-a-skgrepo-and-sends-again ()
  "A refusal naming skgrepos asks for one after the empty stream, and
sends the same receipt with it."
  (test--receipt-run
   (lambda (sent _handlers)
     (skg--receipt-handler
      "mp" "ok" nil nil
      "((response-type receipt) (refusal \"Name the skgrepo.\") (choose-skgrepo (reader-public reader-private)))")
     (should (null (funcall sent)))
     (should skg--rerender-after-empty-stream)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt choices &rest _)
                  (should (equal choices '("reader-public" "reader-private")))
                  "reader-private")))
       (funcall skg--rerender-after-empty-stream))
     (should (string-match-p
              (regexp-quote "(item . \"mp\") (text . \"ok\") (fork-into . \"reader-private\")")
              (car (funcall sent)))))))

(ert-deftest test-receipt-asks-for-a-mailbox-and-sends-again ()
  (test--receipt-run
   (lambda (sent _handlers)
     (skg--receipt-handler
      "m3" "ok" nil nil
      "((response-type receipt) (refusal \"Name the mailbox.\") (choose-mailbox (t u)) (choose-mailbox-titles (\"to reader\" \"also to reader\")))")
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt _choices &rest _) "also to reader (u)")))
       (funcall skg--rerender-after-empty-stream))
     (should (string-match-p
              (regexp-quote "(item . \"m3\") (text . \"ok\") (mailbox . \"u\")")
              (car (funcall sent)))))))

(ert-deftest test-receipt-shows-a-refusal-without-choices ()
  (let ((skg--rerender-after-empty-stream nil))
    (should (equal (test--receipt-message
                    (lambda ()
                      (skg--receipt-handler
                       "x" "ok" nil nil
                       "((response-type receipt) (refusal \"No note has the ID x.\"))")))
                   "skg receipt: No note has the ID x."))
    (should (null skg--rerender-after-empty-stream))))

(ert-deftest test-receipt-is-on-C-c-m-a ()
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c m a"))
              #'skg-receipt)))

(provide 'test-skg-receipt)
