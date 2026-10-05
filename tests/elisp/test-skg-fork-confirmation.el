;;; test-skg-fork-confirmation.el --- Tests for the fork-confirmation client -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-request-save)

(ert-deftest test-save-request-sexp-omits-approved-forks-by-default ()
  "Without approval, the save request carries no approved-forks field."
  (let ((sexp (skg--save-request-sexp
               "view-id-1"
               '(:point-lines-below-focused-headline 0
                 :point-column 0
                 :point-screen-lines-below-window-start 0))))
    (should (equal (cdr (assoc 'request sexp)) "save buffer"))
    (should-not (assoc 'approved-forks sexp))))

(ert-deftest test-save-request-sexp-includes-approved-forks-when-set ()
  "With approval, the save request carries (approved-forks . \"true\")."
  (let ((sexp (skg--save-request-sexp
               "view-id-1"
               '(:point-lines-below-focused-headline 0
                 :point-column 0
                 :point-screen-lines-below-window-start 0)
               t)))
    (should (equal (cdr (assoc 'approved-forks sexp)) "true"))))

(ert-deftest test-save-request-sexp-omits-fork-repos-by-default ()
  "Without chosen repos, the save request carries no fork-repos field."
  (let ((sexp (skg--save-request-sexp
               "view-id-1"
               '(:point-lines-below-focused-headline 0
                 :point-column 0
                 :point-screen-lines-below-window-start 0)
               t)))
    (should-not (assoc 'fork-repos sexp))))

(ert-deftest test-save-request-sexp-includes-fork-repos-when-set ()
  "With chosen repos, the request carries (fork-repos ((N . X) ...))."
  (let* ((sexp (skg--save-request-sexp
                "view-id-1"
                '(:point-lines-below-focused-headline 0
                  :point-column 0
                  :point-screen-lines-below-window-start 0)
                t
                '(("N" . "owned2") ("M" . "owned"))))
         (entry (assoc 'fork-repos sexp)))
    (should entry)
    ;; The field is (fork-repos ((N . X) (M . Y))) -- a list, not a
    ;; dotted pair -- so the alist is the cadr.
    (should (equal (cadr entry)
                   '(("N" . "owned2") ("M" . "owned"))))))

(ert-deftest test-save-request-sexp-carries-exact-hoist-pids ()
  "Hoist authority is a proper PID list, never a broad boolean."
  (let* ((sexp (skg--save-request-sexp
                "view-id-1"
                '(:point-lines-below-focused-headline 0
                  :point-column 0
                  :point-screen-lines-below-window-start 0)
                nil nil '("A" "B")))
         (entry (assoc 'approved-hoist-pids sexp)))
    (should (equal entry '(approved-hoist-pids "A" "B")))))

(ert-deftest test-hoist-confirmation-balances-save-and-retries-exact-pids ()
  "Approving Hoist terminates the first save and preserves fork authority."
  (let ((source (generate-new-buffer "*hoist-origin*"))
        (skg-response-handler-map
         '((save-result ignore . t)
           (collateral-view ignore)
           (save-relax-lock ignore)
           (fork-confirmation ignore)
           (telescope-hoist-confirmation ignore)))
        (skg-lp--pending-count 1)
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
                  ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
                  ((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda (&rest args) (setq called args))))
          (let ((noninteractive nil))
            (skg--telescope-hoist-confirmation-handler
             source
             "((response-type telescope-hoist-confirmation) (telescopes (((pid \"A\") (home \"public\")) ((pid \"B\") (home \"private\")))) (prompt \"Hoist?\"))"
             t '(("N" . "owned")))))
      (kill-buffer source))
    (should (equal called
                   '(t (("N" . "owned")) ("A" "B") nil)))
    (should (= skg-lp--pending-count 0))
    (should-not (assoc 'save-result skg-response-handler-map))))

(ert-deftest test-save-request-sexp-carries-text-release-pids ()
  "Saved/collateral rerender authority uses the shared release field."
  (let* ((sexp (skg--save-request-sexp
                "view-id-1"
                '(:point-lines-below-focused-headline 0
                  :point-column 0
                  :point-screen-lines-below-window-start 0)
                nil nil nil '("U1" "U2")))
         (entry (assoc 'approved-overPrivateText-pids sexp)))
    (should (equal entry '(approved-overPrivateText-pids "U1" "U2")))))

(ert-deftest test-save-text-release-balances-and-retries-exact-pids ()
  "The save is skgsave-committed, but no staged text is adopted before approval."
  (let ((source (generate-new-buffer "*save-release-origin*"))
        (skg-response-handler-map
         '((save-result ignore . t)
           (collateral-view ignore)
           (save-relax-lock ignore)
           (overPrivateText-telescope-confirmation ignore)))
        (skg-lp--pending-count 1)
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
                  ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
                  ((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda (&rest args) (setq called args))))
          (let ((noninteractive nil))
            (skg--save-text-release-confirmation-handler
             source
             "((response-type overPrivateText-telescope-confirmation) (operation save-rerender) (pids (U1 U2)) (prompt \"Include?\"))"
             t '(("N" . "owned")) '("H"))))
      (kill-buffer source))
    (should (equal called
                   '(t (("N" . "owned")) ("H") ("U1" "U2"))))
    (should (= skg-lp--pending-count 0))
    (should-not (assoc 'save-result skg-response-handler-map))))

(ert-deftest test-fork-repos-from-confirmation-buffer-walks-two-levels ()
  "skg--fork-repos-from-confirmation-buffer pairs each clone-to-be
parent's (repo X) with each child's (id N)."
  (with-temp-buffer
    (insert "# FORK CONFIRMATION\n")
    (insert "* (skg (node (repo owned2) (viewStats (homeRepoHerald ⌂:owned2)))) N-edited\n")
    (insert "** (skg (node (id N) (repo foreign) (affectsParent false) writeProtected (rels \"aO\"))) N-original\n")
    (org-mode)
    (should (equal (skg--fork-repos-from-confirmation-buffer)
                   '(("N" . "owned2"))))))

(ert-deftest test-fork-repos-walk-does-not-leak-repo-across-clones ()
  "A metadata-less level-1 headline must not leak the previous clone's
repo to a later fork's child (parent-repo resets on every level 1)."
  (with-temp-buffer
    (insert "* (skg (node (repo ownedA))) A-edited\n")
    (insert "** (skg (node (id N1) (repo foreign) writeProtected)) N1-original\n")
    ;; A stray/garbled level-1 headline with no skg metadata.
    (insert "* plain headline, no metadata\n")
    (insert "** (skg (node (id N2) (repo foreign) writeProtected)) N2-original\n")
    (org-mode)
    ;; N1 -> ownedA; N2 must NOT inherit ownedA (its parent has no skgrepo).
    (should (equal (skg--fork-repos-from-confirmation-buffer)
                   '(("N1" . "ownedA"))))))

(ert-deftest test-show-fork-confirmation-builds-editable-navigable-buffer ()
  "skg--show-fork-confirmation inserts the content into an EDITABLE
content-view buffer (so the user can rotate each clone's repo), records
the source buffer, leaves skg-view-id nil, and binds approve/decline plus an
ordinary-save refusal on C-x C-s."
  (let ((source (generate-new-buffer "*fork-origin*")))
    (unwind-protect
        (let ((buf (skg--show-fork-confirmation
                    "# FORK CONFIRMATION\n* (skg (node (repo owned))) N-edited\n** (skg (node (id N) (repo foreign) (affectsParent false) writeProtected (rels \"aO\"))) N-original\n"
                    source)))
          (unwind-protect
              (with-current-buffer buf
                (should-not buffer-read-only)
                (should (null skg-view-id))
                (should (eq skg--fork-source-buffer source))
                (should (derived-mode-p 'skg-content-view-mode))
                (should (string-match-p "(id N)" (buffer-string)))
                ;; approve / decline / save-refusal are reachable
                (should (eq (key-binding (kbd "C-c C-c")) #'skg-approve-fork))
                (should (eq (key-binding (kbd "C-c C-k")) #'skg-decline-fork))
                (should (eq (key-binding (kbd "C-x C-s"))
                            #'skg--fork-confirmation-refuse-save)))
            (kill-buffer buf)))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest test-approve-fork-replaces-confirmation-pane-with-result ()
  "Approval leaves the confirmation window showing a truthful result buffer."
  (let ((source (generate-new-buffer "*fork-result-origin*"))
        (result-name "*SKG Fork Result*")
        confirm confirm-window called)
    (unwind-protect
        (save-window-excursion
          (when (get-buffer result-name) (kill-buffer result-name))
          (setq confirm
                (skg--show-fork-confirmation
                 "* (skg (node (repo owned))) N-edited\n** (skg (node (id N) (repo foreign) writeProtected)) N-original\n"
                 source))
          (setq confirm-window (get-buffer-window confirm t))
          (should (window-live-p confirm-window))
          (cl-letf (((symbol-function 'skg-request-save-buffer)
                     (lambda (&rest args) (setq called args))))
            (with-current-buffer confirm
              (let ((noninteractive t))
                (skg-approve-fork))))
          (let ((result (get-buffer result-name)))
            (should-not (buffer-live-p confirm))
            (should (buffer-live-p result))
            (should (eq (window-buffer confirm-window) result))
            (with-current-buffer result
              (should (string-match-p
                       "Fork confirmed; saving\\.\\.\\."
                       (buffer-string))))
            (should (equal called '(t (("N" . "owned")))))
            (skg--finish-pending-fork-result
             source "((content \"saved\") (errors ()) (warnings ()))")
            (with-current-buffer result
              (should (string-match-p
                       "Fork confirmed; save successful\\."
                       (buffer-string))))
            (with-current-buffer source
              (should-not skg--pending-fork-result))))
      (when (buffer-live-p confirm) (kill-buffer confirm))
      (when (get-buffer result-name) (kill-buffer result-name))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest test-fork-confirmation-does-not-mutate-shared-mode-map ()
  "The buffer-local key overrides must not leak into the shared
skg-content-view-mode-map (which would break C-x C-s in real views)."
  (let ((source (generate-new-buffer "*fork-origin-3*")))
    (let ((buf (skg--show-fork-confirmation
                "* (skg (node (repo owned))) N-edited\n"
                source)))
      (unwind-protect
          (should (eq (lookup-key skg-content-view-mode-map (kbd "C-x C-s"))
                      #'skg-request-save-buffer))
        (kill-buffer buf)
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest test-fork-choose-placeholder-repos-prompts-with-suggestion ()
  "skg--fork-choose-placeholder-repos prompts once per placeholder
clone, offering the server's suggested repo (the comment above the
clone) as the default, and writes the choice into the metadata."
  (with-temp-buffer
    (insert "* Fork confirmation -- what this buffer is\n")
    (insert "Some explanation.\n")
    (insert "# Suggested repo for the clone below: owned2\n")
    (insert "* (skg (node (repo PICK-A-REPO) (viewStats (homeRepoHerald ⌂:PICK-A-REPO)))) N-edited\n")
    (insert "** (skg (node (id N) (repo foreign) (affectsParent false) writeProtected)) N-original\n")
    (org-mode)
    (let ((offered-defaults nil))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("owned" "owned2")))
                ((symbol-function 'skg--completing-read-with-cycle)
                 (lambda (_prompt _collection _pred _req _init _hist def
                                  &rest _)
                   (push def offered-defaults)
                   def))) ;; the user accepts the default
        (skg--fork-choose-placeholder-repos))
      (should (equal offered-defaults '("owned2")))
      (should (string-match-p "(repo owned2)" (buffer-string)))
      (should (string-match-p "⌂:owned2" (buffer-string)))
      (should-not (string-match-p "(repo PICK-A-REPO)"
                                  (buffer-string))))))

(ert-deftest test-fork-choose-placeholder-repos-skips-specified-clones ()
  "A clone whose repo is already real (the user specified it in the
saved metadata, so the server omitted the placeholder) prompts nothing."
  (with-temp-buffer
    (insert "* (skg (node (repo owned2) (viewStats (homeRepoHerald ⌂:owned2)))) N-edited\n")
    (insert "** (skg (node (id N) (repo foreign) (affectsParent false) writeProtected)) N-original\n")
    (org-mode)
    (cl-letf (((symbol-function 'skg--completing-read-with-cycle)
               (lambda (&rest _)
                 (error "must not prompt for a specified repo"))))
      (skg--fork-choose-placeholder-repos))
    (should (string-match-p "(repo owned2)" (buffer-string)))))

(ert-deftest test-approve-fork-errors-when-origin-is-gone ()
  "skg-approve-fork refuses when the originating buffer is dead."
  (let ((source (generate-new-buffer "*fork-origin-2*")))
    (let ((buf (skg--show-fork-confirmation "* (skg (node (id N) (repo foreign) writeProtected)) N\n"
                                            source)))
      (kill-buffer source) ;; source dies before approval
      (unwind-protect
          (with-current-buffer buf
            (should-error (skg-approve-fork)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'test-skg-fork-confirmation)
