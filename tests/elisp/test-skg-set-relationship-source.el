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

;; --- The recursive walk: skg--set-relationship-source-recursive-walk ---

(defvar test--recursive-content-tree
  (concat
   "* (skg (node (id r) (source public))) r\n"
   "** (skg (node (id a) (source public))) a\n"
   "*** (skg (node (id b) (source public))) b\n"
   "** (skg (node (id c) (source public) (parentIs independent))) c\n"
   "*** (skg (node (id d) (source public))) d\n"
   "** (skg (node (id e) (source public) indef)) e\n"
   "*** (skg (node (id f) (source public))) f\n"
   "** (skg subscribeeCol)\n"
   "*** (skg (node (id g) (source public))) g\n"
   "**** (skg (node (id h) (source public))) h\n"
   "** (skg aliasCol) aliases\n")
  "A view-root tree exercising the walk's qualification and pruning:
affected content (a, b), an independent branch (c, d), an
indefinitive-but-affected member (e) over content (f), a
subscribeeCol member (g) over subscribee-as-such content (h), and an
aliasCol.")

(defun test--line-of-id (id)
  "Return the text of the buffer line whose metadata carries ID."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "(id %s)" id))
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(ert-deftest test-recursive-walk-contained ()
  "Kind `contained' hits affected content children of definitive
activeNode parents only: the root's own (absent) edge is skipped, the
independent branch and everything below the indefinitive node and the
subscribee-as-such member are pruned, and col members are untouched."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((count (skg--set-relationship-source-recursive-walk
                   'contained "trusted")))
       (should (= count 3)) ;; a, b, e
       (dolist (id '("a" "b" "e"))
         (should (string-match-p "(relSource trusted)"
                                 (test--line-of-id id))))
       (dolist (id '("r" "c" "d" "f" "g" "h"))
         (should-not (string-match-p "relSource"
                                     (test--line-of-id id))))))))

(ert-deftest test-recursive-walk-subscribee ()
  "Kind `subscribee' hits only subscribeeCol members, leaving content
children and subscribee-as-such content untouched."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((count (skg--set-relationship-source-recursive-walk
                   'subscribee "private")))
       (should (= count 1)) ;; g
       (should (string-match-p "(relSource private)"
                               (test--line-of-id "g")))
       (dolist (id '("r" "a" "b" "c" "d" "e" "f" "h"))
         (should-not (string-match-p "relSource"
                                     (test--line-of-id id))))))))

(ert-deftest test-recursive-walk-root-edge-inclusive ()
  "Starting the walk below the view root includes the start node's
own edge to its view-parent."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id a)")
     (beginning-of-line)
     (let ((count (skg--set-relationship-source-recursive-walk
                   'contained "trusted")))
       (should (= count 2)) ;; a and b
       (should (string-match-p "(relSource trusted)"
                               (test--line-of-id "a")))
       (should (string-match-p "(relSource trusted)"
                               (test--line-of-id "b")))))))

(ert-deftest test-recursive-walk-overridden-and-member-content ()
  "An overriddenCol member matches kind `overridden'; the member's
own content children (the member being definitive) match kind
`contained' through the col."
  (let ((tree (concat
               "* (skg (node (id anchor) (source public))) anchor\n"
               "** (skg overriddenCol)\n"
               "*** (skg (node (id o) (source public))) o\n"
               "**** (skg (node (id oc) (source public))) oc\n")))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (should (= 1 (skg--set-relationship-source-recursive-walk
                     'overridden "private")))
       (should (string-match-p "(relSource private)"
                               (test--line-of-id "o")))
       (should-not (string-match-p "relSource"
                                   (test--line-of-id "oc")))))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (should (= 1 (skg--set-relationship-source-recursive-walk
                     'contained "private")))
       (should (string-match-p "(relSource private)"
                               (test--line-of-id "oc")))
       (should-not (string-match-p "relSource"
                                   (test--line-of-id "o")))))))

(ert-deftest test-recursive-walk-prunes-readonly-col ()
  "A read-only col's whole branch is pruned: even a definitive
member's content children are not reached."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id owner) (source public))) owner\n"
    "** (skg subscriberCol)\n"
    "*** (skg (node (id s) (source public))) s\n"
    "**** (skg (node (id sc) (source public))) sc\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should (= 0 (skg--set-relationship-source-recursive-walk
                   'contained "trusted")))
     (dolist (id '("s" "sc"))
       (should-not (string-match-p "relSource"
                                   (test--line-of-id id)))))))

(ert-deftest test-recursive-walk-indefinitive-col-anchor ()
  "A writable col under an INDEFINITIVE anchor is not collected at
save, so its members do not match -- whether the walk starts at the
anchor (pruned below the indefinitive node) or at the col itself
(refused by the anchor-definitiveness check)."
  (let ((tree (concat
               "* (skg (node (id anchor) (source public) indef)) anchor\n"
               "** (skg subscribeeCol)\n"
               "*** (skg (node (id g) (source public))) g\n")))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (should (= 0 (skg--set-relationship-source-recursive-walk
                     'subscribee "trusted")))
       (should-not (string-match-p "relSource"
                                   (test--line-of-id "g")))))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (search-forward "subscribeeCol")
       (beginning-of-line)
       (should (= 0 (skg--set-relationship-source-recursive-walk
                     'subscribee "trusted")))
       (should-not (string-match-p "relSource"
                                   (test--line-of-id "g")))))))

(ert-deftest test-recursive-walk-removes-overrides ()
  "The no-override choice removes existing (relSource ...) atoms
throughout the subtree."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (source public))) r\n"
    "** (skg (node (id a) (source public) (viewStats (relSource trusted)))) a\n"
    "*** (skg (node (id b) (source public) (viewStats (relSource private)))) b\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should (= 2 (skg--set-relationship-source-recursive-walk
                   'contained skg--relationship-source-no-override)))
     (dolist (id '("a" "b"))
       (should-not (string-match-p "relSource"
                                   (test--line-of-id id)))))))

;; --- The kind menu: skg--select-relationship-kind ---

(ert-deftest test-relationship-kind-menu-settable-roles ()
  "The menu tree offers exactly the three writable kinds, one per
writable position, and covers all five schema relations."
  (should (equal (mapcar #'car skg--relationship-kind-menu-tree)
                 '("contains" "textlinks_to" "subscribes"
                   "hides_from_its_subscriptions" "overrides_view_of")))
  (should (equal (delq nil
                       (mapcar (lambda (role) (nth 1 role))
                               (apply #'append
                                      (mapcar #'cdr
                                              skg--relationship-kind-menu-tree))))
                 '(contained subscribee overridden))))

(defun test--choose-menu-role (role-line)
  "In the relationship-kind menu buffer, move to ROLE-LINE and choose it."
  (with-current-buffer "*skg-relationship-kinds*"
    (goto-char (point-min))
    (search-forward role-line)
    (beginning-of-line)
    (skg--relationship-kind-menu-choose)))

(ert-deftest test-relationship-kind-menu-choose ()
  "RET on a settable role calls the continuation with its kind; RET
on a read-only role refuses; RET on a relation (level-1) headline
does neither."
  (unwind-protect
      (progn
        (let (chosen)
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) (set-buffer buffer))))
            (skg--select-relationship-kind
             (lambda (kind) (setq chosen kind))))
          (with-current-buffer "*skg-relationship-kinds*"
            (goto-char (point-min))
            (search-forward "* subscribes")
            (beginning-of-line)
            (skg--relationship-kind-menu-choose) ;; level-1: a no-op
            (should-not chosen)
            (should-error (test--choose-menu-role "** subscriber")
                          :type 'user-error))
          (test--choose-menu-role "** contained")
          (should (eq chosen 'contained))
          ;; Choosing killed the menu buffer.
          (should-not (get-buffer "*skg-relationship-kinds*"))))
    (when (get-buffer "*skg-relationship-kinds*")
      (kill-buffer "*skg-relationship-kinds*"))))

(ert-deftest test-set-relationship-source-recursive-end-to-end ()
  "The full command: menu choice, level prompt, walk. Point and
window plumbing are stubbed as in the other handler tests."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((view-buffer (current-buffer)))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'pop-to-buffer)
                        (lambda (buffer &rest _) (set-buffer buffer)))
                       ((symbol-function 'completing-read)
                        (lambda (&rest _) "trusted")))
               (skg-set-relationship-source-recursive)
               (test--choose-menu-role "** contained"))
             (with-current-buffer view-buffer
               (dolist (id '("a" "b" "e"))
                 (should (string-match-p "(relSource trusted)"
                                         (test--line-of-id id))))
               (should-not (string-match-p "relSource"
                                           (test--line-of-id "g")))))
         (when (get-buffer "*skg-relationship-kinds*")
           (kill-buffer "*skg-relationship-kinds*")))))))

;; --- set-source stuck-edge offer and indefinitive warning ---
;; (Here rather than in test-skg-metadata.el because these need the
;; config harness: the stuck-edge analysis reads the privacy ladder.
;; In test--config-public-private-trusted the order is public,
;; private, trusted -- so private -> public publicizes.)

(defun test--messages-during (fn)
  "Run FN with `message' captured; return (RESULT . MESSAGES)."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt
                   (push (apply #'format fmt args) messages))
                 nil)))
      (cons (funcall fn) (nreverse messages)))))

(ert-deftest test-set-source-recursive-offers-stuck-edge-fix ()
  "A publicizing recursive move detects the content edges it would
leave behind, and on acceptance writes (relSource NEW) atoms on them."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (source private))) r\n"
    "** (skg (node (id a) (source private))) a\n"
    "*** (skg (node (id b) (source private))) b\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let (offer-prompt)
       (cl-letf (((symbol-function 'skg--prompt-for-source-change)
                  (lambda (_current) "public"))
                 ((symbol-function 'y-or-n-p)
                  (lambda (prompt) (setq offer-prompt prompt) t)))
         (let ((msgs (cdr (test--messages-during
                           (lambda () (skg-set-source t))))))
           (should (string-match-p "2 content relationships" offer-prompt))
           (should (seq-find (lambda (m)
                               (string-match-p "Also publicized 2" m))
                             msgs))))
       (dolist (id '("r" "a" "b"))
         (should (string-match-p "(source public)"
                                 (test--line-of-id id))))
       (dolist (id '("a" "b"))
         (should (string-match-p "(relSource public)"
                                 (test--line-of-id id))))
       (should-not (string-match-p "relSource"
                                   (test--line-of-id "r")))))))

(ert-deftest test-set-source-recursive-decline-mentions-recursive-relsource ()
  "Declining the stuck-edge offer leaves the edges alone and points
at C-c s R."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (source private))) r\n"
    "** (skg (node (id a) (source private))) a\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-source-change)
                (lambda (_current) "public"))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt) nil)))
       (let ((msgs (cdr (test--messages-during
                         (lambda () (skg-set-source t))))))
         (should (seq-find (lambda (m) (string-match-p "C-c s R" m))
                           msgs))))
     (should-not (string-match-p "relSource" (test--line-of-id "a"))))))

(ert-deftest test-set-source-recursive-no-offer-when-privatizing ()
  "A privatizing move strands nothing (edges rise automatically), so
no offer is made."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (source public))) r\n"
    "** (skg (node (id a) (source public))) a\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-source-change)
                (lambda (_current) "trusted"))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt)
                  (error "Should not offer a stuck-edge fix"))))
       (test--messages-during (lambda () (skg-set-source t))))
     (should (string-match-p "(source trusted)" (test--line-of-id "a")))
     (should-not (string-match-p "relSource" (test--line-of-id "a"))))))

(ert-deftest test-set-source-recursive-warns-about-indefinitive ()
  "An indefinitive matching instance is NOT edited; its ID goes to
*Messages* and the summary carries a loud WARNING. Edges touching
it are not offered (they cannot actually publicize)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (source private))) r\n"
    "** (skg (node (id e) (source private) indef)) e\n"
    "*** (skg (node (id f) (source private))) f\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-source-change)
                (lambda (_current) "public"))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt)
                  (error "Should not offer: both edges touch the indefinitive node"))))
       (let ((msgs (cdr (test--messages-during
                         (lambda () (skg-set-source t))))))
         (should (seq-find (lambda (m)
                             (and (string-match-p "NOT changed" m)
                                  (string-match-p ": e" m)))
                           msgs))
         (should (seq-find (lambda (m)
                             (and (string-match-p "WARNING" m)
                                  (string-match-p "\\*Messages\\*" m)))
                           msgs))))
     (should (string-match-p "(source private)" (test--line-of-id "e")))
     (dolist (id '("r" "f"))
       (should (string-match-p "(source public)"
                               (test--line-of-id id)))))))

(ert-deftest test-set-source-single-offers-direct-child-edges ()
  "A single (non-recursive) publicizing move offers only the edges it
actually changes: its direct children's inbound edges whose default
rises. A child still more private than the new source is left alone."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (source private))) r\n"
    "** (skg (node (id a) (source public))) a\n"
    "** (skg (node (id c) (source trusted))) c\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-source-change)
                (lambda (_current) "public"))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (should (string-match-p "1 content relationship "
                                          prompt))
                  t)))
       (test--messages-during (lambda () (skg-set-source))))
     (should (string-match-p "(source public)" (test--line-of-id "r")))
     ;; a's edge default rose private->public; c's stayed trusted.
     (should (string-match-p "(relSource public)" (test--line-of-id "a")))
     (should-not (string-match-p "relSource" (test--line-of-id "c")))
     (should (string-match-p "(source trusted)" (test--line-of-id "c"))))))

(ert-deftest test-set-source-single-indefinitive-warns-and-skips ()
  "A single move of an indefinitive instance edits nothing and warns."
  (test--with-skg-content-view
   "* (skg (node (id x) (source private) indef)) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-source-change)
                (lambda (_current) "public")))
       (let ((msgs (cdr (test--messages-during
                         (lambda () (skg-set-source))))))
         (should (seq-find (lambda (m) (string-match-p "WARNING" m))
                           msgs))))
     (should (string-match-p "(source private)" (test--line-of-id "x")))
     (should-not (string-match-p "(source public)"
                                 (test--line-of-id "x"))))))

(provide 'test-skg-set-relationship-source)
