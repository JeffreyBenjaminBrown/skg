(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-id-search)
(require 'skg-request-single-root-content-view)

(ert-deftest test-single-root-request-carries-id-and-view-uri ()
  "The request identifies both the node and the new client view."
  (let ((request (skg--single-root-view-request-string
                  "some-id" "some-uri")))
    (should (string-match-p "some-id" request))
    (should (string-match-p "some-uri" request))))

(ert-deftest test-single-root-request-carries-exact-overPrivateText-approvals ()
  "A confirmed retry names the approved PIDs; an ordinary request does not."
  (let ((ordinary (skg--single-root-view-request-string
                   "some-id" "some-uri"))
        (approved (skg--single-root-view-request-string
                   "some-id" "some-uri" '("pid-a" "pid-b"))))
    (should-not (string-match-p "allow-overPrivateText-telescopes" ordinary))
    (should (equal
             '("pid-a" "pid-b")
             (cdr (assoc 'allow-overPrivateText-telescopes (read approved)))))))
