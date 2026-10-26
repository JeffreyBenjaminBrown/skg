;;; test-skg-request-views.el --- Tests for the ghost tombstone requests  -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-metadata)
(require 'skg-request-views)
(require 'skg-keymaps-and-aliases)

(defconst test-skg-request-views--ghost
  "(skg (vnid 0d) ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed)))"
  "A ghost's metadata, as the diff view writes it.")

(defun test-skg-request-views--run (headlines command)
  "Run COMMAND on the last of HEADLINES, with the save stubbed.
Returns (LINES . SAVE-COUNT): the buffer's lines after COMMAND, and
how many saves it requested."
  (with-temp-buffer
    (insert headlines)
    (org-mode)
    (goto-char (point-max))
    (beginning-of-line)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (funcall command))
      (cons (split-string (buffer-string) "\n" t)
            save-count))))

(ert-deftest test-skg-ghost-tombstone-keybindings ()
  "C-c r t and C-c r T are bound to the two tombstone commands."
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c r t"))
              'skg-request-ghost-tombstone))
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c r T"))
              'skg-request-buffer-ghosts-tombstone)))

(ert-deftest test-skg-request-ghost-tombstone-stamps-the-ghost-form-and-saves ()
  "The request lands inside the ghost's form, the rest of the headline
unchanged, and one save is requested."
  (let ((result (test-skg-request-views--run
                 (concat "* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n"
                         "** " test-skg-request-views--ghost " g")
                 #'skg-request-ghost-tombstone)))
    (should (equal (car result)
                   '("* (skg node (gnid r) (skgRepos (nodeRepo main))) r"
                     "** (skg (vnid 0d) ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed)) (viewRequests tombstone)) g")))
    (should (= (cdr result) 1))))

(ert-deftest test-skg-request-ghost-tombstone-refuses-anything-but-a-ghost ()
  "On any other headline it refuses, changes nothing and saves nothing."
  (dolist (headline '("* (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (rel removed))) x"
                      "* (skg unknown (gnid u))"
                      "* (skg tombstone)"
                      "* no metadata"))
    (with-temp-buffer
      (insert headline)
      (org-mode)
      (goto-char (point-min))
      (let ((save-count 0))
        (cl-letf (((symbol-function 'skg-request-save-buffer)
                   (lambda () (setq save-count (1+ save-count)))))
          (should (equal (condition-case err
                             (progn (skg-request-ghost-tombstone) nil)
                           (user-error (cadr err)))
                         "Only a ghost can be entombstoned; the tombstone view request belongs on a ghost (skg-request-ghost-tombstone)")))
        (should (equal (buffer-string) headline))
        (should (= save-count 0))))))

(ert-deftest test-skg-request-buffer-ghosts-tombstone-stamps-only-ghosts-and-saves-once ()
  "Every ghost in the buffer, wherever point is, carries the request
inside its ghost form, once (a ghost behind a marker atom, or already
carrying it, too); no other headline changes; one save is requested."
  (let ((result (test-skg-request-views--run
                 (concat "* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n"
                         "** " test-skg-request-views--ghost " g\n"
                         "*** (skg node (gnid k) (skgRepos (nodeRepo main)) writeProtected) k\n"
                         "** (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim (diff (rel removed))) x\n"
                         "** (skg unknown (gnid u))\n"
                         "** (skg tombstone)\n"
                         "* (skg folded ghost (gnid h) (skgRepos (nodeRepo main)) (viewRequests tombstone)) h")
                 #'skg-request-buffer-ghosts-tombstone)))
    (should (equal (car result)
                   '("* (skg node (gnid r) (skgRepos (nodeRepo main))) r"
                     "** (skg (vnid 0d) ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed)) (viewRequests tombstone)) g"
                     "*** (skg node (gnid k) (skgRepos (nodeRepo main)) writeProtected) k"
                     "** (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim (diff (rel removed))) x"
                     "** (skg unknown (gnid u))"
                     "** (skg tombstone)"
                     "* (skg folded ghost (gnid h) (skgRepos (nodeRepo main)) (viewRequests tombstone)) h")))
    (should (= (cdr result) 1))))

(ert-deftest test-skg-request-buffer-ghosts-tombstone-without-a-ghost-does-not-save ()
  "With no ghost in the buffer, nothing changes and no save is requested."
  (let* ((headlines (concat "* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n"
                            "** (skg tombstone)"))
         (result (test-skg-request-views--run
                  headlines #'skg-request-buffer-ghosts-tombstone)))
    (should (equal (car result) (split-string headlines "\n" t)))
    (should (= (cdr result) 0))))

(provide 'test-skg-request-views)
