(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'heralds-minor-mode)
(require 'skg-request-herald-rules) ;; self-heal: skg-herald-rules-ensure
(skg-test-install-herald-rules)

;; (NAME METADATA TEXT STYLES) for each case in the file, which the
;; Neovim tests read too. STYLES is one style name (or "-") per character.
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
  (let ((cases nil) (name nil) (metadata nil) (text nil))
    (dolist (line (split-string
                   (with-temp-buffer
                     (insert-file-contents test-heralds--shared-cases-file)
                     (buffer-string))
                   "\n"))
      (cond ((string-prefix-p "==== " line)
             (setq name (substring line 5) metadata nil text nil))
            ((string-prefix-p "---- text: " line)
             (setq text (substring line 11)))
            ((string-prefix-p "---- styles: " line)
             (push (list name metadata text
                         (test-heralds--expand-style-runs (substring line 13)))
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
      (let ((display (heralds-from-metadata (nth 1 case))))
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
with ✓ when every such change is ready, and its ordinary label
otherwise. Editing a member rerenders the folder's herald."
  (let ((ordinary (substring-no-properties
                   (heralds-from-metadata "(skg containerFolder)")))
        (changed "☮ its containers changed (other files)")
        (ready-member "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected (diff (rel added) (leaves 1 rel) (ready rel))) c")
        (unready-member "*** (skg node (gnid d) (skgRepos (nodeRepo r)) noClaim writeProtected (diff (rel removed) (leaves 1 rel))) d"))
    (should (equal (test-heralds--folder-display
                    (list "* (skg node (gnid r) (skgRepos (nodeRepo r))) r"
                          "** (skg containerFolder)"
                          ready-member))
                   (concat changed "✓")))
    (should (equal (test-heralds--folder-display
                    (list "* (skg node (gnid r) (skgRepos (nodeRepo r))) r"
                          "** (skg containerFolder)"
                          ready-member
                          unready-member))
                   changed))
    ;; A member whose relRepo moved: changed, ready once the move is.
    (should (equal (test-heralds--folder-display
                    '("** (skg containerFolder)"
                      "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected) c"
                      "**** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))"))
                   changed))
    (should (equal (test-heralds--folder-display
                    '("** (skg containerFolder)"
                      "*** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim writeProtected) c"
                      "**** (skg (relRepoMove (old a) (new b)) (diff (leaves 1) (ready)))"))
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
                    (list "** (skg containerFolder)" unready-member)
                    (lambda ()
                      (search-forward "(leaves 1 rel)")
                      (insert " (ready rel)")))
                   (concat changed "✓")))))

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
        (should (= (length overlays-after-disable) 0)))
      (should ;; Also check that heralds-overlays variable is cleared
       (null heralds-overlays)))))

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
    ;; Major-mode switch kills buffer-local heralds-overlays but
    ;; leaves the actual overlay objects in the buffer.
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
