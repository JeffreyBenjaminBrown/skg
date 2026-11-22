;;; test-skg-describe-heralds.el --- skg-describe-at-point client tests -*- lexical-binding: t; -*-
;;;
;;; The sentences are the server's ('server/herald_descriptions.rs',
;;; whose unit tests check them); these check what the client sends
;;; and how it shows what comes back.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-request-describe-heralds)

(defun test--describe-run (body)
  "Run BODY with the network faked. BODY receives a function returning
the sent requests (newest first) and the handlers by response type."
  (let ((sent nil) (handlers (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers))))
      (funcall body (lambda () sent) handlers))))

(ert-deftest test-describe-sends-the-metadata-of-the-headline-whose-body-point-is-in ()
  (with-temp-buffer
    (insert "* (skg (vnid 1) node (gnid p)) p\n"
            "** (skg (vnid 2) node (gnid c) (view (rels (linksTo (in 5 (substantive 2)))))) c\n"
            "its body\n")
    (org-mode)
    (goto-char (point-max))
    (forward-line -1)
    (test--describe-run
     (lambda (sent handlers)
       (skg-describe-at-point)
       (should (equal (funcall sent)
                      (list (concat
                             "((request . \"describe heralds\") (metadata . "
                             "\"(skg (vnid 2) node (gnid c) (view (rels (linksTo "
                             "(in 5 (substantive 2))))))\"))\n"))))
       (should (gethash 'describe-heralds handlers))))))

(ert-deftest test-describe-adds-a-pending-detachment-and-a-folder-change ()
  (with-temp-buffer
    (insert "* (skg (vnid 1) node (gnid p)) p\n"
            "** (skg (vnid 2) node (gnid c) noClaim) c\n"
            "** (skg (vnid 3) aliasFolder) aliases\n"
            "*** (skg (vnid 4) alias (diff (rel added))) another name\n")
    (org-mode)
    (goto-char (point-min))
    (forward-line 1)
    (let ((request (skg--describe-request-at-point)))
      (should (equal (alist-get 'metadata request)
                     "(skg (vnid 2) node (gnid c) noClaim pendingDetachment)")))
    (forward-line 1)
    (should (equal (alist-get 'folder-change (skg--describe-request-at-point))
                   "unprocessed"))))

(ert-deftest test-describe-refuses-outside-a-headline ()
  (with-temp-buffer
    (insert "before any headline\n* (skg (vnid 1) node (gnid p)) p\n")
    (org-mode)
    (goto-char (point-min))
    (should-error (skg--describe-request-at-point) :type 'user-error)))

(ert-deftest test-describe-reads-lines-or-an-error ()
  (should (equal (skg--describe-lines-of-response
                  "((response-type describe-heralds) (lines \"a\" \"b c\"))")
                 '("a" "b c")))
  (should (equal (skg--describe-lines-of-response
                  "((response-type describe-heralds) (error \"no\"))")
                 '(error "no"))))

(ert-deftest test-describe-shows-two-short-lines-in-the-echo-area-and-more-in-a-buffer ()
  (let ((shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq shown (apply #'format format-string args)))))
      (skg--describe-show '("aK: the headline above it (a) forks this one."))
      (should (equal shown "aK: the headline above it (a) forks this one."))
      (setq shown nil)
      (unwind-protect
          (progn
            (skg--describe-show '("one." "two." "three."))
            (should-not (equal shown "one.\ntwo.\nthree."))
            (with-current-buffer skg-describe-buffer-name
              (should (equal (buffer-string) "one.\n\ntwo.\n\nthree.\n"))))
        (when (get-buffer skg-describe-buffer-name)
          (kill-buffer skg-describe-buffer-name))))))

(provide 'test-skg-describe-heralds)
