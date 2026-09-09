;;; test-skg-save-response-folded-root.el --- Repro for "root vanishes" bug

;; Reproduces the bug where a single-root content view, saved while
;; fully folded (org-startup-folded behavior), comes back with the
;; root headline missing from the buffer text. See Dear_Claude.org
;; and the companion Rust test at tests/save/folded_single_root.rs.
;;
;; The Rust test proves the server round-trip preserves the root, so
;; the suspect is `skg-replace-buffer-with-new-content' and the
;; client-side fold/unfold + focus processing it runs.
;;
;; ROOT CAUSE (exposed by the failing test below):
;; After `skg-fold-marked-headlines' + `skg-remove-folded-markers'
;; run, the root headline is folded (its subtree hidden by an
;; org-fold overlay). Then `skg-remove-focused-marker' finds the
;; root, and `skg-edit-metadata-at-point' calls
;; `skg-replace-current-line', which issues a
;; `delete-region' on (line-beginning-position . line-end-position).
;; Because that region starts at the beginning of a folded heading,
;; org-fold's `org-fold-core--fix-folded-region' expands the delete
;; to cover the entire hidden subtree. The root line (and its
;; children's text) is clobbered, and only the reformatted line is
;; re-inserted at point. The test that has `focused' on the root
;; FAILS; the test without `focused' PASSES — confirming the trigger
;; is the focus-removal step operating on a folded heading.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-org-fold)
(require 'skg-focus)
(require 'skg-request-save)
(require 'skg-request-text-search)

(defun skg-test--buffer-text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest test-save-response-with-folded-children-preserves-root ()
  "After a folded single-root save, the server's response carries the
root with `focused' metadata and each child with `folded' metadata.
`skg-replace-buffer-with-new-content' must NOT drop the root line."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "root body\n"
           "** (skg folded (node (id c1))) c1\n"
           "c1 body\n"
           "** (skg folded (node (id c2))) c2\n"
           "c2 body\n"))
         (buf (generate-new-buffer "*test-save-response-folded-root*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content nil from-rust)
          (let ((actual (skg-test--buffer-text)))
            (should (string-match-p "(id root)" actual))
            (should (string-match-p "^\\* " actual))
            (should (string-match-p "(id c1)" actual))
            (should (string-match-p "(id c2)" actual))))
      (kill-buffer buf))))

(ert-deftest test-save-response-no-focused-preserves-root ()
  "Same as above but without a `focused' marker on the root, to
isolate the fold-processing path from the focus-processing path."
  (let* ((from-rust
          (concat
           "* (skg (node (id root))) root\n"
           "root body\n"
           "** (skg folded (node (id c1))) c1\n"
           "c1 body\n"
           "** (skg folded (node (id c2))) c2\n"
           "c2 body\n"))
         (buf (generate-new-buffer "*test-save-response-folded-root-nofocus*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content nil from-rust)
          (let ((actual (skg-test--buffer-text)))
            (should (string-match-p "(id root)" actual))
            (should (string-match-p "^\\* " actual))))
      (kill-buffer buf))))

(ert-deftest test-save-response-restores-point-in-focused-body ()
  "Point returns to the same body line below the focused headline."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "body 1\n"
           "body 2\n"
           "** (skg (node (id child))) child\n"))
         (buf (generate-new-buffer "*test-save-response-body-point*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content
           nil
           from-rust
           '(:point-lines-below-focused-headline 2
             :point-screen-lines-below-window-start 0))
          (should (string= "body 2"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-leaves-point-on-focused-headline-when-body-line-vanishes ()
  "If the saved body line is gone, point stays on the focused headline."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "** (skg (node (id child))) child\n"))
         (buf (generate-new-buffer "*test-save-response-vanished-body-point*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content
           nil
           from-rust
           '(:point-lines-below-focused-headline 2
             :point-screen-lines-below-window-start 0))
          (should (string-match-p
                   "^\\* .*root$"
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-restores-column-within-body-line ()
  "Point returns to the same character within the restored body line."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "body 1\n"
           "body 2\n"
           "** (skg (node (id child))) child\n"))
         (buf (generate-new-buffer "*test-save-response-column*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content
           nil
           from-rust
           '(:point-lines-below-focused-headline 2
             :point-column 4
             :point-screen-lines-below-window-start 0))
          (should (string= "body 2"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
          (should (= 4 (- (point) (line-beginning-position)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-clamps-column-to-end-of-shortened-line ()
  "A saved column past the end of a now-shorter line clamps to the
line's end and does not spill onto the next line."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "ab\n"
           "** (skg (node (id child))) child\n"))
         (buf (generate-new-buffer "*test-save-response-clamp-column*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content
           nil
           from-rust
           '(:point-lines-below-focused-headline 1
             :point-column 10
             :point-screen-lines-below-window-start 0))
          (should (string= "ab"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
          (should (= (line-end-position) (point))))
      (kill-buffer buf))))

(ert-deftest test-save-response-clamps-line-to-last-line-of-entry ()
  "When the entry shrinks so the saved line offset would reach another
headline, point lands on the entry's last line, not on the next
headline and not back on the focused headline."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "only body line\n"
           "** (skg (node (id child))) child\n"))
         (buf (generate-new-buffer "*test-save-response-clamp-line*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content
           nil
           from-rust
           '(:point-lines-below-focused-headline 5
             :point-column 0
             :point-screen-lines-below-window-start 0))
          (should (string= "only body line"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-success-with-warnings-shows_warning_channel ()
  (let ((skg--server-session-id "11111111-2222-4333-8444-555555555555")
        (buf (generate-new-buffer "*test-save-response-warning*"))
        (shown nil))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (cl-letf (((symbol-function 'skg-replace-buffer-with-new-content)
                     (lambda (_tcp-proc new-content &optional _position
                              _authority)
                       (erase-buffer)
                       (insert new-content)))
                    ((symbol-function 'skg-big-nonfatal-message)
                     (lambda (buffer-name message-text content)
                       (setq shown
                             (list buffer-name message-text content)))))
            (skg-handle-save-sexp
             (prin1-to-string
              '((content "* root\n")
                (errors ())
                (warnings ("audit warning"))
                (view-write-authority editable)
                (server-session-id "11111111-2222-4333-8444-555555555555"))))
            (should (string= (buffer-string) "* root\n"))
            (should (equal (car shown) "*SKG Save Warnings*"))
            (should (string-match-p "^\\* warnings\n\\*\\* audit warning"
                                    (nth 2 shown)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-advances-new-buffer-graph-baseline ()
  "A new view opened after a save inherits the save's selected generation."
  (let ((skg--server-session-id "11111111-2222-4333-8444-555555555555")
        (saved (generate-new-buffer "*test-save-generation-saved*"))
        (later (generate-new-buffer "*test-save-generation-later*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-store-state '((graph-generation . 1))))
    (unwind-protect
        (progn
          (with-current-buffer saved
            (org-mode)
            (setq-local skg-view-uri "saved-uri")
            (insert "* (skg (node (id root))) root\n")
            (skg-register-buffer
             saved 'content-view :lifecycle 'live-view :disposable nil
             :view-uri skg-view-uri :last-fetched (buffer-string)
             :graph-generation 1 :presentation-generation 0
             :server-revision 0 :application-token 1)
            (skg-handle-save-sexp
             (prin1-to-string
              '((content "* (skg (node (id root))) root\n")
                (errors ()) (warnings ()) (root-ids (root))
                (graph-generation 2) (presentation-generation 0)
                (server-revision 1) (client-application-token 2)
                (view-write-authority editable)
                (server-session-id "11111111-2222-4333-8444-555555555555")))))
          (should (= 2 (alist-get 'graph-generation
                                  skg--server-store-state)))
          (with-current-buffer later
            (org-mode)
            (insert "* later\n")
            (skg-register-buffer
             later 'new-empty-content-view
             :lifecycle 'live-view :disposable nil :view-uri "later-uri"
             :last-fetched (buffer-string))
            (should (= 2 (skg--buffer-record-graph-generation
                          skg--buffer-record))))
          (skg-observe-server-graph-generation 1)
          (should (= 2 (alist-get 'graph-generation
                                  skg--server-store-state))))
      (kill-buffer saved)
      (kill-buffer later))))

(ert-deftest test-background-offer-normalizes-unquoted-wire-atoms ()
  "Symbol-shaped wire atoms still match string-valued buffer authority."
  (let ((skg--server-session-id "11111111-2222-4333-8444-555555555555")
        (buf (generate-new-buffer "*test-background-wire-atoms*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-store-state '((graph-generation . 2)))
        (skg--active-source-set-name "all")
        acknowledgement)
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "view-uri"
                      skg--application-token 2)
          (insert "* (skg (node (id root))) root\n")
          (skg-register-buffer
           buf 'content-view :lifecycle 'live-view :disposable nil
           :view-uri skg-view-uri :last-fetched (buffer-string)
           :graph-generation 2 :presentation-generation 0
           :server-revision 1 :application-token 2)
          (set-buffer-modified-p nil)
          (cl-letf (((symbol-function 'skg-register-response-handler)
                     (lambda (&rest _)))
                    ((symbol-function 'skg-submit-request)
                     (lambda (_tcp request &rest _)
                       (setq acknowledgement (read request)))))
            (skg--background-collateral-offer-handler
             nil
             "((operation-id collateral-1) (view-uri view-uri)\
 (content \"* (skg (node (id root))) updated\\n\")\
 (warnings ()) (graph-generation 3) (presentation-generation 0)\
 (viewforest-base-revision 1) (resulting-server-revision 2)\
 (view-base-graph-generation 2)\
 (view-base-presentation-generation 0)\
 (expected-client-application-token 2)\
 (view-write-authority editable)\
 (resulting-client-application-token 3)\
 (view-base-source-set all) (resulting-source-set all)\
 (server-session-id \"11111111-2222-4333-8444-555555555555\"))"))
          (should (string-match-p "updated" (buffer-string)))
          (should (= 3 (skg--buffer-record-graph-generation
                        skg--buffer-record)))
          (should (equal "true" (cdr (assoc 'applied acknowledgement))))
          (should (equal "view-uri"
                         (cdr (assoc 'view-uri acknowledgement)))))
      (kill-buffer buf))))

(ert-deftest test-search-enrichment-normalizes-unquoted-wire-atoms ()
  "Symbol-shaped wire atoms still match string-valued search authority."
  (let ((skg--server-session-id "11111111-2222-4333-8444-555555555555")
        (buf (generate-new-buffer "*test-search-wire-atoms*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-store-state '((graph-generation . 2)))
        (skg--active-source-set-name "all")
        acknowledgement)
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "search:bravo"
                      skg--application-token 2)
          (insert "* (skg (node (id root))) root\n")
          (skg-register-buffer
           buf 'search-view :lifecycle 'live-view :disposable nil
           :view-uri skg-view-uri :last-fetched (buffer-string)
           :graph-generation 2 :presentation-generation 0
           :server-revision 1 :application-token 2)
          (set-buffer-modified-p nil)
          (cl-letf (((symbol-function 'skg-register-response-handler)
                     (lambda (&rest _)))
                    ((symbol-function 'skg-submit-request)
                     (lambda (_tcp request &rest _)
                       (setq acknowledgement (read request)))))
            (skg--display-search-enrichment
             nil
             (format
              "((operation-id search-1) (view-uri search:bravo)\
 (client-buffer-id %s) (terms \"bravo\")\
 (content \"* (skg (node (id root))) enriched\\n\")\
 (warnings ()) (graph-generation 3) (presentation-generation 0)\
 (viewforest-base-revision 1) (resulting-server-revision 2)\
 (view-base-graph-generation 2)\
 (view-base-presentation-generation 0)\
 (expected-client-application-token 2)\
 (view-write-authority editable)\
 (resulting-client-application-token 3)\
 (view-base-source-set all) (resulting-source-set all)\
 (server-session-id \"11111111-2222-4333-8444-555555555555\"))"
              (skg--buffer-record-id skg--buffer-record))))
          (should (string-match-p "enriched" (buffer-string)))
          (should (= 3 (skg--buffer-record-graph-generation
                        skg--buffer-record)))
          (should (equal "true" (cdr (assoc 'applied acknowledgement))))
          (should (equal "search:bravo"
                         (cdr (assoc 'view-uri acknowledgement)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-failure-with-errors-and-warnings-shows_both ()
  (let ((skg--server-session-id "11111111-2222-4333-8444-555555555555")
        (buf (generate-new-buffer "*test-save-response-errors-warnings*"))
        (shown nil)
        (replaced nil))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (cl-letf (((symbol-function 'skg-replace-buffer-with-new-content)
                     (lambda (&rest _args)
                       (setq replaced t)))
                    ((symbol-function 'skg-big-nonfatal-message)
                     (lambda (buffer-name message-text content)
                       (setq shown
                             (list buffer-name message-text content)))))
            (skg-handle-save-sexp
             (prin1-to-string
              '((content nil)
                (errors ("fatal save error"))
                (warnings ("audit warning"))
                (view-write-authority editable)
                (server-session-id "11111111-2222-4333-8444-555555555555"))))
            (should-not replaced)
            (should (equal (car shown) "*SKG Save Errors and Warnings*"))
            (should (string-match-p "^\\* errors\n\\*\\* fatal save error"
                                    (nth 2 shown)))
            (should (string-match-p "^\\* warnings\n\\*\\* audit warning"
                                    (nth 2 shown)))))
      (kill-buffer buf))))

(provide 'test-skg-save-response-folded-root)
