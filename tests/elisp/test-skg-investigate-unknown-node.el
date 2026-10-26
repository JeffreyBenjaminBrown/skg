;;; test-skg-investigate-unknown-node.el --- Investigate-unknown-node client tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-request-investigate-unknown-node)

(defun test--investigate-run (typed body)
  "Run BODY with the network faked and `read-string' answering TYPED.
BODY receives the sent requests (newest first, via a function), the
handlers, and the prompts (via a function)."
  (let ((sent nil) (prompts nil) (handlers (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers)))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt prompts)
                 typed)))
      (funcall body (lambda () sent) handlers (lambda () prompts)))))

(ert-deftest test-investigate-sends-the-id-of-the-unknown-headline-at-point ()
  (with-temp-buffer
    (insert "* (skg node (gnid p) (skgRepos (nodeRepo main))) p\n"
            "** (skg unknown (gnid gone-id)) \n")
    (org-mode)
    (goto-char (point-min))
    (forward-line 1)
    (test--investigate-run
     "not used"
     (lambda (sent handlers prompts)
       (skg-investigate-unknown-node)
       (should (null (funcall prompts)))
       (should (equal (funcall sent)
                      '("((request . \"investigate unknown node\") (id . \"gone-id\"))\n")))
       (should (gethash 'investigate-unknown-node handlers))))))

(ert-deftest test-investigate-prompts-for-the-id-elsewhere ()
  (with-temp-buffer
    (insert "* (skg node (gnid p) (skgRepos (nodeRepo main))) p\n")
    (org-mode)
    (goto-char (point-min))
    (test--investigate-run
     "typed-id"
     (lambda (sent _handlers prompts)
       (skg-investigate-unknown-node)
       (should (equal (funcall prompts) '("Investigate ID: ")))
       (should (string-match-p "(id \\. \"typed-id\")"
                               (car (funcall sent))))))))

(ert-deftest test-investigate-shows-the-answer-in-a-read-only-org-buffer ()
  (unwind-protect
      (progn
        (skg--investigate-unknown-node-handler
         nil
         (prin1-to-string '((content "* gone-id\n** never present in the git history of any skgrepo\n")
                            (errors ())
                            (warnings ()))))
        (with-current-buffer "*skg unknown node*"
          (should (derived-mode-p 'org-mode))
          (should buffer-read-only)
          (should skg-report-mode)
          (should (string-match-p "never present"
                                  (buffer-string)))))
    (when (get-buffer "*skg unknown node*")
      (kill-buffer "*skg unknown node*"))))

(provide 'test-skg-investigate-unknown-node)
