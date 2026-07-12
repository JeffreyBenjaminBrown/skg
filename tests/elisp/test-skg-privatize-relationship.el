;;; test-skg-privatize-relationship.el --- Tests for skg-privatize-relationship
;;;
;;; skg-privatize-relationship (C-c s r) cycles the (relSource ...)
;;; metadata atom on the headline at point: [no atom, then each
;;; configured source in config order], wrapping back to "no atom"
;;; (which REMOVES the atom -- the only way to undo a privatization).
;;; It refuses on a member of a read-only col.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-metadata)
(require 'skg-config)

(defvar test--config-public-private-trusted
  (concat "[[sources]]\n"
          "name = \"public\"\n"
          "path = \"owned/public\"\n"
          "\n"
          "[[sources]]\n"
          "name = \"private\"\n"
          "path = \"owned/private\"\n"
          "\n"
          "[[sources]]\n"
          "name = \"trusted\"\n"
          "path = \"owned/trusted\"\n"
          "")
  "Config text with three owned sources, in this declared order:
public, private, trusted. (The names don't need to reflect an actual
privacy order for these tests -- only that `skg--source-names'
returns them in this config order, which is all the client-side
cycle depends on; the server enforces the real floor.)")

(defun test--with-skg-content-view (org-text config-text body-fn)
  "Run BODY-FN in a temp skg content-view buffer with ORG-TEXT.
CONFIG-TEXT is written to a temporary skgconfig.toml so that
skg-config-dir is set and `skg--source-names' works."
  (let* ((config-dir (make-temp-file "skg-test-config" t))
         (config-file (expand-file-name "skgconfig.toml" config-dir))
         (skg-config-dir (file-name-as-directory config-dir)))
    (with-temp-file config-file
      (insert config-text))
    (unwind-protect
        (with-temp-buffer
          (insert org-text)
          (skg-content-view-mode)
          (goto-char (point-min))
          (funcall body-fn))
      (delete-file config-file)
      (delete-directory config-dir))))

(defun test--buffer-line (n)
  "Return the text of line N (1-indexed) of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- n))
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

;; --- The cycle: no atom -> public -> private -> trusted -> no atom ---

(ert-deftest test-privatize-relationship-cycles-through-all-sources ()
  "Repeated calls cycle [no atom, public, private, trusted, no atom]."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should-not (string-match-p "relSource" (test--buffer-line 1)))

     (skg-privatize-relationship)
     (should (string-match-p "(relSource public)" (test--buffer-line 1)))

     (skg-privatize-relationship)
     (should (string-match-p "(relSource private)" (test--buffer-line 1)))
     (should-not (string-match-p "relSource public" (test--buffer-line 1)))

     (skg-privatize-relationship)
     (should (string-match-p "(relSource trusted)" (test--buffer-line 1)))

     ;; Wrapping back to "no atom" REMOVES the relSource atom (and
     ;; the now-empty viewStats form) entirely.
     (skg-privatize-relationship)
     (should-not (string-match-p "relSource" (test--buffer-line 1)))
     (should-not (string-match-p "viewStats" (test--buffer-line 1)))
     (should (string-match-p
              "^\\* (skg (node (id x) (source public))) x$"
              (test--buffer-line 1))))))

;; --- Coexistence with other viewStats content ---

(ert-deftest test-privatize-relationship-preserves-existing-viewstats ()
  "Cycling relSource must not disturb a pre-existing viewStats sibling."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public) (viewStats cycle))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (skg-privatize-relationship)
     (should (string-match-p "cycle" (test--buffer-line 1)))
     (should (string-match-p "(relSource public)" (test--buffer-line 1))))))

;; --- Refusal on read-only cols ---

(ert-deftest test-privatize-relationship-refuses-on-readonly-col-member ()
  "Refuses (user-error) on a member of a read-only col (subscriberCol)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg subscriberCol)\n"
    "*** (skg (node (id sub) (source public))) sub\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id sub)" nil t)
     (beginning-of-line)
     (let ((err (should-error (skg-privatize-relationship)
                               :type 'user-error)))
       (should (string-match-p "read-only" (cadr err)))
       (should (string-match-p "subscriberCol" (cadr err))))
     ;; Buffer is untouched.
     (should-not (string-match-p "relSource" (test--buffer-line 3))))))

(ert-deftest test-privatize-relationship-refuses-on-hiddenCol-member ()
  "Refuses on hiddenCol members too (not just subscriberCol)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg hiddenCol)\n"
    "*** (skg (node (id victim) (source public))) victim\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id victim)" nil t)
     (beginning-of-line)
     (should-error (skg-privatize-relationship) :type 'user-error))))

;; --- Allowed on a WRITABLE col member (subscribeeCol) ---

(ert-deftest test-privatize-relationship-allowed-on-writable-col-member ()
  "A subscribeeCol member (WritableSet policy) may be privatized."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg subscribeeCol)\n"
    "*** (skg (node (id seen) (source public) indef)) seen\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id seen)" nil t)
     (beginning-of-line)
     (skg-privatize-relationship)
     (should (string-match-p "(relSource public)" (test--buffer-line 3))))))

;; --- Refusal off a headline / off an activeNode ---

(ert-deftest test-privatize-relationship-refuses-off-activeNode ()
  "Refuses on a scaffold headline (no (node ...) form)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg aliasCol) aliases\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(skg aliasCol)" nil t)
     (beginning-of-line)
     (should-error (skg-privatize-relationship) :type 'user-error))))

;; --- Helper unit tests ---

(ert-deftest test-privatize-relationship-current-value-reads-relsource ()
  "skg--privatize-relationship-current-value reads the raw metadata."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public) (viewStats (relSource private)))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should (equal (skg--privatize-relationship-current-value) "private")))))

(ert-deftest test-privatize-relationship-current-value-nil-when-absent ()
  "Returns nil when no relSource atom is present."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public) (viewStats cycle))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should-not (skg--privatize-relationship-current-value)))))

(ert-deftest test-privatize-relationship-parent-readonly-col-atom ()
  "skg--privatize-relationship-parent-readonly-col-atom detects each
read-only col kind and returns nil for a writable one."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg overriderCol)\n"
    "*** (skg (node (id o) (source public))) o\n"
    "** (skg subscribeeCol)\n"
    "*** (skg (node (id s) (source public) indef)) s\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id o)" nil t)
     (beginning-of-line)
     (should (eq (skg--privatize-relationship-parent-readonly-col-atom)
                 'overriderCol))
     (search-forward "(id s)" nil t)
     (beginning-of-line)
     (should-not (skg--privatize-relationship-parent-readonly-col-atom)))))

(provide 'test-skg-privatize-relationship)
