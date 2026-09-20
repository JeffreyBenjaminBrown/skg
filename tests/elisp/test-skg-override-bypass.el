(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-id-search)
(require 'skg-request-single-root-content-view)

(ert-deftest test-single-root-request-default-has-no-override-choice ()
  "Without the bypass flag, the request carries no override-choice
field: the server defaults to opening the requested node raw."
  (let ((request (skg--single-root-view-request-string
                  "some-id" "some-uri" nil)))
    (should-not (string-match-p "override-choice" request))
    (should (string-match-p "some-id" request))))

(ert-deftest test-single-root-request-bypass-field ()
  "The legacy bypass flag remains representable for old servers."
  (let ((request (skg--single-root-view-request-string
                  "some-id" "some-uri" t)))
    (should (string-match-p
             (regexp-quote "(override-choice . \"bypass\")")
             request))))

(ert-deftest test-single-root-request-carries-exact-overPrivateText-approvals ()
  "A confirmed retry names the approved PIDs; an ordinary request does not."
  (let ((ordinary (skg--single-root-view-request-string
                   "some-id" "some-uri" nil))
        (approved (skg--single-root-view-request-string
                   "some-id" "some-uri" nil '("pid-a" "pid-b"))))
    (should-not (string-match-p "allow-overPrivateText-telescopes" ordinary))
    (should (equal
             '("pid-a" "pid-b")
             (cdr (assoc 'allow-overPrivateText-telescopes (read approved)))))))

(ert-deftest test-goto-bypassOverride-is-a-compatibility-alias ()
  "The old command follows the same raw-visit path as `skg-goto'."
  (let ((called nil))
    (cl-letf (((symbol-function 'skg-goto)
               (lambda () (interactive) (setq called t))))
      (call-interactively #'skg-goto-bypassOverride))
    (should called)))
