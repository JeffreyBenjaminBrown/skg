(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-id-search)
(require 'skg-request-single-root-content-view)

(ert-deftest test-single-root-request-default-is-menu ()
  "Without the bypass flag, the request carries no override-choice
field: the server defaults to offering the override-choice menu."
  (let ((request (skg--single-root-view-request-string
                  "some-id" "some-uri" nil)))
    (should-not (string-match-p "override-choice" request))
    (should (string-match-p "some-id" request))))

(ert-deftest test-single-root-request-bypass-field ()
  "With the bypass flag, the request carries
\(override-choice . \"bypass\")."
  (let ((request (skg--single-root-view-request-string
                  "some-id" "some-uri" t)))
    (should (string-match-p
             (regexp-quote "(override-choice . \"bypass\")")
             request))))

(ert-deftest test-bypass-detection-in-magit-buffers ()
  "A goto from a magit buffer bypasses the menu; from other
buffers it does not. Detection is by major-mode name, so magit
need not be loaded."
  (with-temp-buffer
    (setq-local major-mode 'magit-status-mode)
    (should (skg--bypass-override-here-p)))
  (with-temp-buffer
    (setq-local major-mode 'org-mode)
    (should-not (skg--bypass-override-here-p)))
  (with-temp-buffer
    (setq-local major-mode 'fundamental-mode)
    (should-not (skg--bypass-override-here-p))))
