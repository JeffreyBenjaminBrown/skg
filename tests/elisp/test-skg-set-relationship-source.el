;;; test-skg-set-relationship-source.el --- Tests for skg-set-relationship-source
;;;
;;; skg-set-relationship-source (C-c s r; formerly
;;; skg-privatize-relationship, see
;;; BUG-and-fix_make-edge-more-public.org) classifies the edge the
;;; headline at point represents, asks the server for that edge's
;;; (default, current) privacy levels, and offers the levels at least
;;; as private as the default plus a no-override choice. These tests
;;; cover the pure pieces (edge classification, menu slicing, choice
;;; application) and the response handler with the network and
;;; minibuffer stubbed out.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-metadata)
(require 'skg-config)
(require 'skg-request-edge-level-info)

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
returns them in this config order, which is all the client-side menu
depends on; the server enforces the real floor at save.)")

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

;; --- Edge classification: skg--relationship-edge-at-point ---

(ert-deftest test-relationship-edge-content-child ()
  "A content child's edge: owner = org-parent, relation = contains."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg (node (id kid) (source public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (should (equal (skg--relationship-edge-at-point)
                    '(:owner "owner" :member "kid"
                      :relation "contains"))))))

(ert-deftest test-relationship-edge-writable-col-member ()
  "A subscribeeCol member's edge: owner = the col's ANCHOR (its
org-parent), relation = the col's relation."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id anchor) (source public))) anchor\n"
    "** (skg subscribeeCol)\n"
    "*** (skg (node (id seen) (source public) indef)) seen\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id seen)" nil t)
     (beginning-of-line)
     (should (equal (skg--relationship-edge-at-point)
                    '(:owner "anchor" :member "seen"
                      :relation "subscribes_to"))))))

(ert-deftest test-relationship-edge-refuses-on-readonly-col-member ()
  "Refuses (user-error) on a member of a read-only col."
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
     (let ((err (should-error (skg--relationship-edge-at-point)
                              :type 'user-error)))
       (should (string-match-p "read-only" (cadr err)))
       (should (string-match-p "subscriberCol" (cadr err)))))))

(ert-deftest test-relationship-edge-refuses-on-root ()
  "Refuses on a root headline: with no org-parent there is no edge."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((err (should-error (skg--relationship-edge-at-point)
                              :type 'user-error)))
       (should (string-match-p "Root headline" (cadr err)))))))

(ert-deftest test-relationship-edge-refuses-off-activeNode ()
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
     (should-error (skg--relationship-edge-at-point)
                   :type 'user-error))))

;; --- Menu slicing: skg--relationship-source-choices ---

(ert-deftest test-relationship-source-choices-slices-at-default ()
  "The menu is the ladder's tail from the default onward."
  (should (equal (skg--relationship-source-choices
                  '("public" "private" "trusted") "private")
                 '("private" "trusted")))
  (should (equal (skg--relationship-source-choices
                  '("public" "private" "trusted") "public")
                 '("public" "private" "trusted")))
  (should (equal (skg--relationship-source-choices
                  '("public" "private" "trusted") "trusted")
                 '("trusted"))))

(ert-deftest test-relationship-source-choices-full-ladder-fallback ()
  "With no default (or one absent from the ladder), the whole ladder
is offered; the server's save-time floor check backstops."
  (should (equal (skg--relationship-source-choices
                  '("public" "private") nil)
                 '("public" "private")))
  (should (equal (skg--relationship-source-choices
                  '("public" "private") "unknown")
                 '("public" "private"))))

;; --- Choice application: skg--apply-relationship-source-choice ---

(ert-deftest test-apply-relationship-source-sets-atom ()
  "Choosing a source writes the (relSource NAME) atom."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (skg--apply-relationship-source-choice "trusted")
     (should (string-match-p "(relSource trusted)"
                             (test--buffer-line 1))))))

(ert-deftest test-apply-relationship-source-preserves-existing-viewstats ()
  "Setting relSource must not disturb a pre-existing viewStats sibling."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public) (viewStats cycle))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (skg--apply-relationship-source-choice "public")
     (should (string-match-p "cycle" (test--buffer-line 1)))
     (should (string-match-p "(relSource public)"
                             (test--buffer-line 1))))))

(ert-deftest test-apply-relationship-source-removes-override ()
  "The no-override choice removes an existing (relSource ...) atom,
and its message says the SAVED level survives (sticky), not that
anything resets to the default."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public) (viewStats (relSource private)))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((msg (skg--apply-relationship-source-choice
                 skg--relationship-source-no-override)))
       (should (string-match-p "sticky" msg))
       (should-not (string-match-p "relSource" (test--buffer-line 1)))))))

(ert-deftest test-apply-relationship-source-remove-without-atom-is-noop ()
  "The no-override choice without an atom changes nothing."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((before (test--buffer-line 1))
           (msg (skg--apply-relationship-source-choice
                 skg--relationship-source-no-override)))
       (should (string-match-p "nothing to remove" msg))
       (should (equal (test--buffer-line 1) before))))))

;; --- The response handler, network and minibuffer stubbed ---

(defun test--run-info-handler (payload choice-fn)
  "Run `skg--set-relationship-source-from-info' on PAYLOAD against
the buffer at point, with `run-at-time' made synchronous and
`completing-read' (which `skg--completing-read-with-cycle' wraps)
stubbed by CHOICE-FN, which receives (PROMPT CHOICES PREFILL) --
PREFILL being the initial minibuffer contents -- and returns the
choice."
  (let ((buffer (current-buffer))
        (marker (point-marker)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args) (apply fn args)))
              ((symbol-function 'completing-read)
               (lambda (prompt choices &optional _pred _req init
                        _hist _def _inherit)
                 (funcall choice-fn prompt choices init))))
      (skg--set-relationship-source-from-info buffer marker payload))))

(ert-deftest test-relationship-source-handler-slices-and-applies ()
  "A (default, current) reply offers the slice from the default plus
the no-override entry, pre-fills the minibuffer with the current
level, and applies the selection."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg (node (id kid) (source public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (let (seen-choices seen-prefill)
       (test--run-info-handler
        "((response-type edge-level-info) (default \"private\") (current \"trusted\"))"
        (lambda (_prompt choices prefill)
          (setq seen-choices choices
                seen-prefill prefill)
          "private"))
       (should (equal seen-choices
                      (list "private" "trusted"
                            skg--relationship-source-no-override)))
       (should (equal seen-prefill "trusted"))
       (should (string-match-p "(relSource private)"
                               (test--buffer-line 2)))))))

(ert-deftest test-relationship-source-handler-below-default-current-prefills-default ()
  "A below-default CURRENT (the foreign shape) is not among the
choices, so the prompt pre-fills with the default instead."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg (node (id kid) (source public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (let (seen-prefill)
       (test--run-info-handler
        "((response-type edge-level-info) (default \"private\") (current \"public\"))"
        (lambda (_prompt _choices prefill)
          (setq seen-prefill prefill)
          skg--relationship-source-no-override))
       (should (equal seen-prefill "private"))))))

(ert-deftest test-relationship-source-handler-error-offers-full-ladder ()
  "An error reply falls back to the full ladder (plus no-override)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg (node (id kid) (source public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (let (seen-choices)
       (test--run-info-handler
        "((response-type edge-level-info) (error \"member 'kid' is not in the graph\"))"
        (lambda (_prompt choices _def)
          (setq seen-choices choices)
          skg--relationship-source-no-override))
       (should (equal seen-choices
                      (list "public" "private" "trusted"
                            skg--relationship-source-no-override)))
       (should-not (string-match-p "relSource"
                                   (test--buffer-line 2)))))))

(provide 'test-skg-set-relationship-source)
