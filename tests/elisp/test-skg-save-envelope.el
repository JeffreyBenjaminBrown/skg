;;; test-skg-save-envelope.el --- A save's envelope carries the saved buffer's baseline -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-request-save)

(ert-deftest test-skg-save-envelope-carries-the-baseline ()
  "The envelope's (baseline TEXT) sits between the saved buffer and the
other views, and survives quoting."
  (should (equal (skg--serialize-save-envelope
                  "* edited \"λ\"\n" "* drawn \"λ\"\n" nil)
                 (concat "((saved-buffer \"* edited \\\"λ\\\"\n\") "
                         "(baseline \"* drawn \\\"λ\\\"\n\") "
                         "(other-views ()))"))))

(ert-deftest test-skg-a-save-sends-the-text-the-server-last-drew ()
  "A save of an edited view sends, as its baseline, the view's text as
it was drawn, not as it was edited."
  (let ((buf (skg-open-org-buffer-from-text
              nil "* (skg node (gnid a)) a\n" "*test-save-envelope*"
              "view-envelope"))
        (sent nil))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-max))
          (let ((inhibit-read-only t)
                (skg--inhibit-dirty-view-confirmation t))
            (insert "a body line\n"))
          (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                     (lambda () 'fake-proc))
                    ((symbol-function 'process-send-string)
                     (lambda (_proc text) (push text sent)))
                    ((symbol-function 'skg-lp-reset) #'ignore)
                    ((symbol-function 'skg-register-response-handler)
                     #'ignore))
            (goto-char (point-min))
            (skg--send-save-buffer nil nil nil nil nil))
          (let ((envelope (read (car sent))))
            (should (equal (cadr (assq 'baseline envelope))
                           "* (skg node (gnid a)) a\n"))
            (should (string-match-p
                     "a body line"
                     (cadr (assq 'saved-buffer envelope))))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(ert-deftest test-skg-a-view-without-a-baseline-cannot-be-saved ()
  "Every view a client can save has a baseline; a buffer without one
is refused locally, before anything is sent."
  (with-temp-buffer
    (setq-local skg-clean-baseline nil)
    (should-error (skg--saved-buffer-baseline))))

(provide 'test-skg-save-envelope)
