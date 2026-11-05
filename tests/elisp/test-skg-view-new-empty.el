;;; test-skg-view-new-empty.el --- Tests for blank new content views -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-view-new-empty)

(defun test--view-new-empty-run (body)
  "Run BODY with the network faked. BODY receives the sent requests
(newest first, via a function) and the registered handlers."
  (let ((sent nil) (handlers (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers))))
      (funcall body (lambda () sent) handlers))))

(ert-deftest test-skg-view-new-empty-opens-blank-content-view ()
  "The new-view command asks the server for the runtime generation,
and opens the blank view when it answers, rendered at that generation.
It must not invent a node or ask for its repo."
  (let (opened)
    (unwind-protect
        (cl-letf (((symbol-function 'skg--prompt-for-owned-repo)
                   (lambda ()
                     (ert-fail "A blank view must not prompt for a repo"))))
          (test--view-new-empty-run
           (lambda (sent handlers)
             (skg-view-new-empty)
             (should (equal (funcall sent)
                            '("((request . \"runtime generation\"))\n")))
             (should-not (get-buffer "*skg-empty*"))
             (funcall (gethash 'runtime-generation handlers)
                      'fake-proc
                      "((response-type runtime-generation) (content \"1762000000000042\"))")))
          (setq opened (current-buffer))
          (should (equal (buffer-name) "*skg-empty*"))
          (should (equal (buffer-string) ""))
          (should (derived-mode-p 'skg-content-view-mode))
          (should (stringp skg-view-id))
          (should (equal skg-rendered-at "1762000000000042"))
          ;; Its baseline is the text it was created with, which its
          ;; save carries.
          (should (equal skg-clean-baseline "")))
      (when (buffer-live-p opened)
        (set-buffer-modified-p nil)
        (kill-buffer opened)))))

(provide 'test-skg-view-new-empty)
