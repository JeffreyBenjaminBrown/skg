(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'heralds-minor-mode)
(require 'skg-request-herald-rules) ;; self-heal: skg-herald-rules-ensure
(require 'skg-request-views)      ;; the view-request gestures
(require 'skg-request-flag-state) ;; skg--stamp-search-matching-request
(skg-test-install-herald-rules)

;; (NAME METADATA TEXT STYLES CONTEXT) for each case in the file, which
;; the Neovim tests read too. STYLES is one style name (or "-") per
;; character; CONTEXT is the text of its '---- context: ' line, or nil.
(defconst test-heralds--shared-cases-file
  (expand-file-name "../shared/herald-rendering-cases.txt"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun test-heralds--expand-style-runs (runs)
  "Expand RUNS, a string of STYLE*COUNT runs, into one style per character."
  (apply #'append
         (mapcar (lambda (run)
                   (let ((parts (split-string run "\\*")))
                     (make-list (string-to-number (cadr parts)) (car parts))))
                 (split-string runs " " t))))

(defun test-heralds--shared-cases ()
  "Parse `test-heralds--shared-cases-file'."
  (let ((cases nil) (name nil) (metadata nil) (text nil) (context nil))
    (dolist (line (split-string
                   (with-temp-buffer
                     (insert-file-contents test-heralds--shared-cases-file)
                     (buffer-string))
                   "\n"))
      (cond ((string-prefix-p "==== " line)
             (setq name (substring line 5) metadata nil text nil context nil))
            ((string-prefix-p "---- context: " line)
             (setq context (substring line 14)))
            ((string-prefix-p "---- text: " line)
             (setq text (substring line 11)))
            ((string-prefix-p "---- styles: " line)
             (push (list name metadata text
                         (test-heralds--expand-style-runs (substring line 13))
                         context)
                   cases)
             (setq name nil))
            ((and name (not metadata)) (setq metadata line))))
    (nreverse cases)))

(defun test-heralds--style-of-face (face)
  "The style name of herald FACE heralds-STYLE-face, or \"-\" for none."
  (if (and face (string-match "\\`heralds-\\(.*\\)-face\\'" (symbol-name face)))
      (match-string 1 (symbol-name face))
    "-"))

(ert-deftest test-heralds-shared-rendering-cases ()
  "Metadata renders as the text and styles of the cases shared with Neovim."
  (let ((cases (test-heralds--shared-cases)))
    (should (> (length cases) 40))
    (dolist (case cases)
      (let ((display (heralds-from-metadata
                      (nth 1 case)
                      (pcase (nth 4 case)
                        ("folderChange processed" 'processed)
                        ("folderChange unprocessed" 'unprocessed))
                      (equal (nth 4 case) "pendingDetachment"))))
        (should (equal (list (car case)
                             (substring-no-properties display)
                             (cl-loop for i below (length display)
                                      collect (test-heralds--style-of-face
                                               (get-text-property i 'face display))))
                       (list (car case) (nth 2 case) (nth 3 case))))))))

(defun test-heralds--folder-display (lines &optional edit)
  "The display text of the folder headline among LINES, with heralds
on, after EDIT (a function called with point at the buffer's start),
if given."
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity lines "\n") "\n")
    (heralds-minor-mode 1)
    (when edit
      (goto-char (point-min))
      (funcall edit))
    (goto-char (point-min))
    (re-search-forward "(skg [a-zA-Z]*Folder)")
    (let ((ov (cl-find-if (lambda (ov) (overlay-get ov 'display))
                          (overlays-at (match-beginning 0)))))
      (should ov)
      (substring-no-properties (overlay-get ov 'display)))))

(ert-deftest test-heralds-a-folder-is-changed-by-its-members ()
  "The server writes a folder as its bare atom. Its headline renders
\"its ... changed ...\" when a member carries a change of its
relationship (a '(diff (rel ...))', or a '(relRepoMove ...)' child),
with ✓ when every such change is processed, and its ordinary label
otherwise. Editing a member rerenders the folder's herald."
  (let ((ordinary (substring-no-properties
                   (heralds-from-metadata "(skg containerFolder)")))
        (changed "☮ its other containers changed (other files)")
        (processed-member "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected (diff (rel added) (leaves 1 rel) (processed rel))) c")
        (unprocessed-member "*** (skg node (gnid d) (skgRepos (nodeRepo r)) noClaim writeProtected (diff (rel removed) (leaves 1 rel))) d"))
    (should (equal (test-heralds--folder-display
                    (list "* (skg node (gnid r) (skgRepos (nodeRepo r))) r"
                          "** (skg containerFolder)"
                          processed-member))
                   (concat changed "✓")))
    (should (equal (test-heralds--folder-display
                    (list "* (skg node (gnid r) (skgRepos (nodeRepo r))) r"
                          "** (skg containerFolder)"
                          processed-member
                          unprocessed-member))
                   changed))
    ;; A member whose relRepo moved: changed, processed once the move is.
    (should (equal (test-heralds--folder-display
                    '("** (skg containerFolder)"
                      "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected) c"
                      "**** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))"))
                   changed))
    (should (equal (test-heralds--folder-display
                    '("** (skg containerFolder)"
                      "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected) c"
                      "**** (skg (relRepoMove (old a) (new b)) (diff (leaves 1) (processed)))"))
                   (concat changed "✓")))
    ;; No member's relationship changed: the ordinary label. A
    ;; grandchild's '(rel ...)' is no member's.
    (should (equal (test-heralds--folder-display
                    '("** (skg containerFolder)"
                      "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected (diff (node added) (leaves 1 node))) c"
                      "**** (skg node (gnid g) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) g"
                      "** (skg node (gnid s) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) sibling"))
                   ordinary))
    ;; Editing a member's line rerenders its folder.
    (should (equal (test-heralds--folder-display
                    (list "** (skg containerFolder)" unprocessed-member)
                    (lambda ()
                      (search-forward "(leaves 1 rel)")
                      (insert " (processed rel)")))
                   (concat changed "✓")))))

(defun test-heralds--last-line-display (lines)
  "The display text of the herald on the last of LINES, with heralds on."
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity lines "\n") "\n")
    (heralds-minor-mode 1)
    (goto-char (point-max))
    (forward-line -1)
    (search-forward "(skg")
    (let ((ov (cl-find-if (lambda (ov) (overlay-get ov 'display))
                          (overlays-at (match-beginning 0)))))
      (and ov (substring-no-properties (overlay-get ov 'display))))))

(ert-deftest test-heralds-a-pending-detachment-renders-as-a-request ()
  "A child that is 'noClaim' and warranted under an editable parent (a
node that is not write-protected, or an editable folder whose
viewparent is one) shows \"detach\". Under a write-protected parent or folder, or
unwarranted, it shows no request."
  (let ((child "** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) c")
        (editable "* (skg node (gnid r) (skgRepos (nodeRepo r))) r")
        (protected "* (skg node (gnid r) (skgRepos (nodeRepo r)) writeProtected) r"))
    (should (equal (test-heralds--last-line-display (list editable child))
                   "detach"))
    (should-not (test-heralds--last-line-display (list protected child)))
    (should (equal (test-heralds--last-line-display
                    (list editable
                          "** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim (view noWarrant)) c"))
                   "⊥"))
    (should-not (test-heralds--last-line-display
                 (list "** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) a root")))
    (should (equal (test-heralds--last-line-display
                    (list editable "** (skg aliasFolder) aliases"
                          "*** (skg alias noClaim) an alias"))
                   "alias detach"))
    (should (equal (test-heralds--last-line-display
                    (list protected "** (skg aliasFolder) aliases"
                          "*** (skg alias noClaim) an alias"))
                   "alias"))
    (should (equal (test-heralds--last-line-display
                    (list editable "** (skg containerFolder) containers"
                          "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected) c"))
                   "☮"))))

;; Every request a gesture stages for the next save shows in the
;; herald of its headline before the save ('docs/heralds.org',
;; "Other heralds"). Each test below runs the gesture itself on the last
;; headline of a buffer whose heralds are on, and reads that
;; headline's herald.

(defconst test-heralds--editable-root
  "* (skg node (gnid r) (skgRepos (nodeRepo main)) (view repoBoundary)) r"
  "An editable root in the skgrepo 'main', labeled \"m\".")

(defconst test-heralds--child-at-home
  "** (skg node (gnid c) (skgRepos (nodeRepo main))) c"
  "A child of `test-heralds--editable-root' in its skgrepo, so drawn
with no home herald.")

(defun test-heralds--last-line-display-after (lines gesture)
  "The herald of the last of LINES, with heralds on, before and after
GESTURE runs with point at the start of that line, as (BEFORE AFTER).
The gestures that save are kept from saving."
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity lines "\n") "\n")
    (heralds-minor-mode 1)
    (let ((display
           (lambda ()
             (save-excursion
               (goto-char (point-max))
               (forward-line -1)
               (search-forward "(skg")
               (let ((ov (cl-find-if (lambda (ov) (overlay-get ov 'display))
                                     (overlays-at (match-beginning 0)))))
                 (and ov (substring-no-properties
                          (overlay-get ov 'display))))))))
      (list (funcall display)
            (progn
              (goto-char (point-max))
              (forward-line -1)
              (cl-letf (((symbol-function 'skg-request-save-buffer) #'ignore))
                (funcall gesture))
              (funcall display))))))

(defun test-heralds--shows-after (lines gesture expected)
  "Assert that GESTURE on the last of LINES makes its herald show
EXPECTED, which it did not show before."
  (let ((before-and-after
         (test-heralds--last-line-display-after lines gesture)))
    (should-not (and (car before-and-after)
                     (string-match-p (regexp-quote expected)
                                     (car before-and-after))))
    (should (cadr before-and-after))
    (should (string-match-p (regexp-quote expected)
                            (cadr before-and-after)))))

(ert-deftest test-heralds-a-pending-home-skgrepo-move-shows ()
  "C-c s s on a child in its parent's skgrepo, which the server drew with
no home herald, shows the move as \"⌂:LABEL\" before the save."
  (should (equal (test-heralds--last-line-display-after
                  (list test-heralds--editable-root test-heralds--child-at-home)
                  (lambda ()
                    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                               (lambda (_current) "pub")))
                      (skg-set-repo))))
                 '(nil "⌂:public"))))

(ert-deftest test-heralds-a-pending-home-skgrepo-move-shows-on-a-boundary ()
  "C-c s s on a root, drawn at a home skgrepo boundary already, shows
the new skgrepo's label in place of the old."
  (should (equal (test-heralds--last-line-display-after
                  (list test-heralds--editable-root)
                  (lambda ()
                    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                               (lambda (_current) "pub")))
                      (skg-set-repo))))
                 '("⌂:m" "⌂:public"))))

(ert-deftest test-heralds-a-pending-recursive-home-skgrepo-move-shows ()
  "C-c s S shows the move on every headline it moves."
  (with-temp-buffer
    (org-mode)
    (insert test-heralds--editable-root "\n"
            test-heralds--child-at-home "\n")
    (heralds-minor-mode 1)
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
               (lambda (_current) "pub")))
      (skg-set-repo-recursive))
    (goto-char (point-min))
    (dolist (_line '(root child))
      (search-forward "(skg")
      (should (equal (substring-no-properties
                      (overlay-get (cl-find-if (lambda (ov) (overlay-get ov 'display))
                                               (overlays-at (match-beginning 0)))
                                   'display))
                     "⌂:public"))
      (forward-line 1))))

(ert-deftest test-heralds-a-pending-attribution-strip-shows ()
  (test-heralds--shows-after
   (list test-heralds--editable-root test-heralds--child-at-home)
   #'skg-strip-attribution "request:strip attribution"))

(ert-deftest test-heralds-a-pending-delete-shows ()
  (test-heralds--shows-after
   (list test-heralds--editable-root test-heralds--child-at-home)
   #'skg-delete "delete"))

(ert-deftest test-heralds-a-pending-merge-shows ()
  (test-heralds--shows-after
   (list test-heralds--editable-root test-heralds--child-at-home)
   (lambda () (skg-set-merge-request "acquiree")) "merge:acquiree"))

(ert-deftest test-heralds-a-pending-relRepo-request-shows ()
  "The request 'skg-set-relRepo' writes, on a node, an alias and an
unknown viewnode."
  (let ((write (lambda () (skg--write-edit-request '(relRepo pub)))))
    (test-heralds--shows-after
     (list test-heralds--editable-root test-heralds--child-at-home)
     write "request:~pub")
    (test-heralds--shows-after
     (list test-heralds--editable-root "** (skg aliasFolder) aliases"
           "*** (skg alias) an alias")
     write "request:~pub")
    (test-heralds--shows-after
     (list test-heralds--editable-root "** (skg unknown (gnid u)) u")
     write "request:~pub")))

(ert-deftest test-heralds-a-pending-flag-request-shows ()
  (test-heralds--shows-after
   (list test-heralds--editable-root test-heralds--child-at-home)
   (lambda () (skg--stamp-search-matching-request t))
   "request:no search matching")
  (test-heralds--shows-after
   (list test-heralds--editable-root test-heralds--child-at-home)
   (lambda () (skg--stamp-search-matching-request nil))
   "request:search matching"))

(ert-deftest test-heralds-a-pending-detach-shows ()
  (test-heralds--shows-after
   (list test-heralds--editable-root test-heralds--child-at-home)
   #'skg-detach "detach"))

(ert-deftest test-heralds-a-pending-restore-node-shows ()
  (test-heralds--shows-after
   (list test-heralds--editable-root
         "** (skg ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed) (leaves 2 node rel))) g")
   #'skg-restore-deleted-node "restore node"))

(ert-deftest test-heralds-a-pending-restore-membership-shows ()
  (test-heralds--shows-after
   (list test-heralds--editable-root
         "** (skg node (gnid c) (skgRepos (nodeRepo main)) noClaim (diff (rel removed) (leaves 1 rel))) c")
   #'skg-restore-removed-membership "restore membership"))

(ert-deftest test-heralds-a-pending-processed-request-shows ()
  "What the processed gestures write, before the save they start."
  (test-heralds--shows-after
   (list test-heralds--editable-root
         "** (skg node (gnid c) (skgRepos (nodeRepo main)) (diff (node added) (leaves 1 node))) c")
   (lambda () (skg-edit-metadata-at-point
               '(skg (editRequest (processed node)))))
   "request:processed:node"))

(ert-deftest test-heralds-a-pending-view-request-shows ()
  "Each view request, from the gesture that writes it."
  (dolist (gesture-and-text
           '((skg-request-editable-view . "req:editable")
             (skg-request-write-protected . "req:write-protected")
             (skg-request-write-protected-recursive . "req:write-protected-subtree")
             (skg-show-folderOf-containers . "req:folder:containers")
             (skg-show-folderOf-flags . "req:flags")))
    (test-heralds--shows-after
     (list test-heralds--editable-root test-heralds--child-at-home)
     (car gesture-and-text) (cdr gesture-and-text)))
  ;; 'skg-fork-node' saves at once; the request stays in the buffer
  ;; while the fork confirmation is open, and the metadata editor
  ;; (C-c v m) can write it without saving.
  (test-heralds--shows-after
   (list test-heralds--editable-root test-heralds--child-at-home)
   (lambda () (skg-edit-metadata-at-point '(skg node (viewRequests fork))))
   "req:fork"))

(ert-deftest test-heralds-minor-mode-toggle ()
  "Test that heralds-minor-mode properly adds and removes overlays."
  (with-temp-buffer
    (progn ;; Insert test text with heralds
      (insert "Test line with (skg node (gnid 123) (view (rels (contains (out 2))) cycle)) herald\n")
      (insert "Another line (skg node (gnid 456) (editRequest delete) (view (rels (linksTo (in 3 (substantive 3)))))) more text\n")
      (insert "Plain line without heralds\n"))
    (progn ;; what happens upon enabling heralds-minor-mode
      (heralds-minor-mode 1)
      (let ;; Check that overlays were created
          ((overlays-after-enable (overlays-in (point-min) (point-max))))
        (should (> (length overlays-after-enable) 0))
        (should (cl-some (lambda (ov) (overlay-get ov 'display))
                         overlays-after-enable))
        (message "After enabling: %d overlays found"
                 (length overlays-after-enable))))
    (progn ;; what happens upon disabling it
      (heralds-minor-mode -1) ;; disable
      (let ;; Check that overlays were removed
          ((overlays-after-disable
            (overlays-in (point-min) (point-max))))
        (setq overlays-after-disable ;; Filter to only overlays with 'display property
              (cl-remove-if-not (lambda (ov) (overlay-get ov 'display))
                                overlays-after-disable))
        (message "After disabling: %d overlays with display property found"
                 (length overlays-after-disable))
        (should (= (length overlays-after-disable) 0))))))

(ert-deftest test-heralds-props-render-with-or-without-rels ()
  "The property counts in '(view (props ...))' render after the
relation tokens, as one herald: the rule table draws a sentinel for
'(rels ...)' and another for '(props ...)', and the second leaves no
trace. Without '(rels ...)' the counts still show."
  (should (equal (substring-no-properties
                  (heralds-from-metadata
                   "(skg node (gnid x) (view (rels (contains (out 3))) (props (aliases 2) (flags 1))))"))
                 "C3 A2 F1"))
  (should (equal (substring-no-properties
                  (heralds-from-metadata
                   "(skg node (gnid x) (view (props (extraIds 1))))"))
                 "I1")))

(ert-deftest test-heralds-minor-mode-visual-check ()
  "The relationship heralds render from semantic facts with per-token
faces, the ⊥/⟳/delete heralds appear, the sentinel placeholder never
leaks, and the overlay clears on disable. The injected node's rels
payload -- (contains (in 2 (ancestors 1))), birth contains -- renders as
the C token 2aC: the in-side numeral \"2\" (medium), the ancestor \"a\"
at its floor (low), and the birth letter \"C\" (low)."
  (with-temp-buffer
    (insert "Line with (skg node (gnid 123) noClaim (editRequest delete) (view (rels (contains (in 2 (ancestors 1)))) (birth (contains in 1)) noWarrant cycle)) text")
    (progn ;; what happens upon enabling heralds-minor-mode
      (heralds-minor-mode 1)
      (let* ( ( herald-start
                ( save-excursion
                  ( goto-char ( point-min ))
                  ( search-forward "(skg " )
                  ( match-beginning 0 )) )
              ( display-overlay
                ( cl-find-if ( lambda ( ov ) ( overlay-get ov 'display ))
                             ( overlays-at herald-start ))) )
        (should display-overlay)
        (should ( stringp ( overlay-get display-overlay 'display )) )
        (let ( ( display-text ( overlay-get display-overlay 'display )) )
          ;; the sentinel placeholder must never reach the display
          ( should-not ( string-match-p "__RELS_SPANS__" display-text ))
          ( should ( string-match "⊥" display-text ))
          ( should ( string-match "2aC" display-text ))
          ( should ( string-match "⟳" display-text ))
          ( should ( string-match "delete" display-text ))
          ;; per-span faces on the 2aC relationship token
          (let ( ( i ( string-match "2aC" display-text )) )
            ( should ( eq ( get-text-property i 'face display-text )
                          'heralds-medium-face )) ;; the "2"
            ( should ( eq ( get-text-property (+ i 1) 'face display-text )
                          'heralds-low-face )) ;; the "a"
            ( should ( eq ( get-text-property (+ i 2) 'face display-text )
                          'heralds-low-face )) )))) ;; the "C"
    (progn ;; what happens upon disabling it
      (heralds-minor-mode -1)
      (let* ( ( herald-start
                ( save-excursion
                  ( goto-char ( point-min ))
                  ( search-forward "(skg " )
                  ( match-beginning 0 )) )
              ( display-overlay
                ( cl-find-if ( lambda ( ov ) ( overlay-get ov 'display ))
                             ( overlays-at herald-start ))) )
        ( should-not display-overlay )) )) )

(ert-deftest test-heralds-survive-major-mode-switch ()
  "After a major-mode switch orphans overlays, disabling heralds
should still remove them."
  (with-temp-buffer
    (insert "(skg node (gnid 1) (skgRepos (nodeRepo s)) (view (rels (contains (out 2)))))\n")
    (heralds-minor-mode 1)
    ;; Overlays exist
    (should (cl-some (lambda (ov) (overlay-get ov 'heralds))
                     (overlays-in (point-min) (point-max))))
    ;; Major-mode switch kills buffer-local variables but leaves
    ;; the actual overlay objects in the buffer.
    (text-mode)
    (should (cl-some (lambda (ov) (overlay-get ov 'heralds))
                     (overlays-in (point-min) (point-max))))
    ;; Switch back and disable — should clear orphans.
    (org-mode)
    (heralds-minor-mode 1)
    (heralds-minor-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'heralds))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest test-heralds-self-heals-missing-rule-table ()
  "Enabling heralds with a missing table re-fetches, then displays.
The herald table is volatile session state; if it goes missing, turning
heralds on should recover it rather than give up. Here the stubbed
fetcher stands in for the server answering the re-request by installing
the fixture table."
  (let ((heralds--transform-rules nil)) ;; pretend the table was lost
    (cl-letf (((symbol-function 'skg-request-herald-rules)
               (lambda () (skg-test-install-herald-rules))))
      (with-temp-buffer
        (insert "(skg node (gnid 1) (skgRepos (nodeRepo s)) (view (rels (contains (out 2)))))\n")
        (heralds-minor-mode 1)
        (should heralds-minor-mode)       ;; stayed on
        (should heralds--transform-rules) ;; table recovered
        (should (cl-some (lambda (ov) (overlay-get ov 'display))
                         (overlays-in (point-min) (point-max))))))))

(ert-deftest test-heralds-gives-up-after-bounded-fetch-attempts ()
  "When the server never sends a table, heralds retries a bounded number
of times and then disables itself instead of spinning forever."
  (let ((heralds--transform-rules nil)
        (skg-rust-tcp-proc nil)             ;; no live connection to wait on
        (skg-herald-rules-attempt-timeout 0.05) ;; keep the test quick
        (calls 0))
    (cl-letf (((symbol-function 'skg-request-herald-rules)
               ;; Simulate a server that never answers: count the
               ;; requests but never install a table.
               (lambda () (setq calls (1+ calls)))))
      (with-temp-buffer
        (insert "(skg node (gnid 1))\n")
        (heralds-minor-mode 1)
        (should-not heralds-minor-mode)     ;; turned itself off
        (should-not heralds--transform-rules)
        (should (= calls skg-herald-rules-max-attempts)) ;; tried exactly N times
        (should-not (cl-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-in (point-min) (point-max))))))))

(provide 'test-heralds-minor-mode)
