(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'heralds-minor-mode)
(require 'skg-request-herald-rules) ;; self-heal: skg-herald-rules-ensure
(skg-test-install-herald-rules)

(ert-deftest test-herald-fractions-and-substitution-styles ()
  "L and C use the same subset grammar; ĥ is distinct from ancestor h."
  (dolist (case '(("(textlinksTo (in 5 (interesting 2)) (out 3))" . "2/5L3")
                  ("(textlinksTo (in 5 (interesting 0)) (out 3))" . "5L3")
                  ("(textlinksTo (in 5 (interesting 2)))" . "2/5L")
                  ("(textlinksTo (out 3))" . "L3")
                  ("(textlinksTo (in 5 (interesting 5)))" . "5/L")
                  ("(textlinksTo (in 1 (ancestors 1) (interesting 1 (ancestors 1))))" . "a/L")
                  ("(textlinksTo (in 2 (ancestors 1 2) (interesting 1 (ancestors 1))))" . "a/bL")
                  ("(textlinksTo (in 5 (ancestors 1) (interesting 1 (ancestors 1))))" . "a/5L")
                  ("(textlinksTo (in 5 (ancestors 1) (interesting 1)))" . "1/5aL")
                  ("(textlinksTo (in 1 (ancestors 1) (interesting 0)))" . "aL")
                  ("(textlinksTo (out 1 (ancestors 1)))" . "La")
                  ("(textlinksTo (out 3 (ancestors 1)))" . "L3a")
                  ("(textlinksTo (in 1 (ancestors 27) (interesting 1 (ancestors 27))))" . "{27}/L")
                  ("(contains (in 2) (out 8 (unintegrated 2)))" . "2C2/8")
                  ("(contains (out 8 (unintegrated 0)))" . "C8")
                  ("(contains (out 8 (unintegrated 8)))" . "C8/")
                  ("(contains (in 2) (out 0 (unintegrated 0)))" . "2C")))
    (let ((display (heralds-from-metadata
                    (format "(skg (node (id x) (rels %s)))" (car case)))))
      (should (equal (substring-no-properties display) (cdr case)))))
  (let* ((display (heralds-from-metadata
                   "(skg (node (id x) (rels (overrides (out 1 (ancestors 8)))) (viewStats (overridesHere y))))"))
         (plain (substring-no-properties display)))
    (should (equal plain "Oĥh"))
    (should (eq (get-text-property 1 'face display)
                'heralds-confusable-face))
    (should (eq (get-text-property 2 'face display)
                'heralds-moderately-interesting-face)))
  (let ((display (heralds-from-metadata
                  "(skg (node (id x) (rels (textlinksTo (in 5 (interesting 2)) (out 3)) (overrides (out 1)) (birth overrides))))")))
    (should (equal (substring-no-properties display) "2/5L3 O1"))
    (should (eq (get-text-property 0 'face display) 'heralds-highly-interesting-face))
    (dolist (i '(1 2 3 4))
      (should (eq (get-text-property i 'face display) 'heralds-blue-face)))
    (should (eq (get-text-property 6 'face display) 'heralds-birth-face))
    (should (eq (get-text-property 7 'face display) 'heralds-highly-interesting-face)))
  (let ((display (heralds-from-metadata
                  "(skg (node (id x) (rels (textlinksTo (in 2 (ancestors 1 2) (interesting 1 (ancestors 1)))))))")))
    (should (equal (substring-no-properties display) "a/bL"))
    (should (eq (get-text-property 0 'face display) 'heralds-slightly-interesting-face))
    (should (eq (get-text-property 2 'face display) 'heralds-moderately-interesting-face))))

(ert-deftest test-herald-birth-face-covers-only-relation-letter ()
  "Counts and fraction slashes beside a birth letter keep their own faces."
  (dolist (case '(((contains (in 1) (out 2)) (birth contains)
                  "1C2" (heralds-blue-face heralds-birth-face heralds-blue-face))
                 ((textlinksTo (in 5 (interesting 2)) (out 3))
                  (birth textlinksTo) "2/5L3"
                  (heralds-highly-interesting-face heralds-blue-face
                   heralds-blue-face heralds-birth-face heralds-blue-face))
                 ((contains (out 8 (unintegrated 2))) (birth contains)
                  "C2/8" (heralds-birth-face heralds-highly-interesting-face
                   heralds-blue-face heralds-blue-face))
                 ((subscribes (in 1) (out 2)) (birth subscribes)
                  "1S2" (heralds-purple-face heralds-birth-face heralds-purple-face))
                 ((overrides (in 1) (out 2)) (birth overrides)
                  "1O2" (heralds-highly-interesting-face heralds-birth-face
                   heralds-highly-interesting-face))
                 ((hides (in 1) (out 2)) (birth hides)
                  "1H2" (heralds-purple-face heralds-birth-face heralds-purple-face))))
    (let ((display (heralds-from-metadata
                    (format "(skg (node (id x) (rels %s %s)))"
                            (prin1-to-string (car case))
                            (prin1-to-string (cadr case))))))
      (should (equal (substring-no-properties display) (nth 2 case)))
      (cl-loop for face in (nth 3 case) for index from 0
               do (should (eq (get-text-property index 'face display)
                              face))))))

(ert-deftest test-heralds-minor-mode-toggle ()
  "Test that heralds-minor-mode properly adds and removes overlays."
  (with-temp-buffer
    (progn ;; Insert test text with herald markers
      (insert "Test line with (skg (node (id 123) (rels (contains (out 2))) (viewStats cycle))) herald\n")
      (insert "Another line (skg (node (id 456) (rels (textlinksTo (in 3 (interesting 3)))) (editRequest delete))) more text\n")
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

(ert-deftest test-heralds-minor-mode-visual-check ()
  "The relationship heralds render from SEMANTIC facts with per-token
faces, the ⊥/⟳/delete heralds appear, the sentinel placeholder never
leaks, and the overlay clears on disable. The injected node's rels
payload -- (contains (in 2 (ancestors 1))), birth contains -- renders as
the C token 2aC: the multi-contains \"2\" (yellow), the ancestor \"a\"
(muted yellow), and the birth \"C\" (black-on-white)."
  (with-temp-buffer
    (insert "Line with (skg (node (id 123) (affectsParent false) (rels (contains (in 2 (ancestors 1))) (birth contains)) (viewStats cycle) (editRequest delete))) text")
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
                          'heralds-highly-interesting-face )) ;; the "2"
            ( should ( eq ( get-text-property (+ i 1) 'face display-text )
                          'heralds-slightly-interesting-face )) ;; the "a"
            ( should ( eq ( get-text-property (+ i 2) 'face display-text )
                          'heralds-birth-face )) )))) ;; the "C"
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

(ert-deftest test-heralds-viewrequests-display ()
  "Test that viewRequests are displayed as req:* heralds."
  (with-temp-buffer
    ;; Test (folder aliases) viewRequest
    (erase-buffer)
    (insert "(skg (node (id 1) (viewRequests (folder aliases))))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:folder:.*aliases" result)))

    ;; Test (path container) viewRequest
    (erase-buffer)
    (insert "(skg (node (id 2) (viewRequests (path container))))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:path:.*container" result)))

    ;; Test (path linkSource) viewRequest
    (erase-buffer)
    (insert "(skg (node (id 3) (viewRequests (path linkSource))))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:path:.*linkSource" result)))

    ;; Test multiple viewRequests
    (erase-buffer)
    (insert "(skg (node (id 4) (viewRequests (folder aliases) (path container) (path linkSource))))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:folder:.*aliases" result))
      (should (string-match "req:path:.*container" result))
      (should (string-match "req:path:.*linkSource" result)))))

(ert-deftest test-heralds-property-count-display ()
  "True file properties are summarized as one cyan Pn herald."
  (let ((result (heralds-from-metadata
                 "(skg (node (id 1) (rels (properties 2))))")))
    (should (equal (substring-no-properties result) "P2"))
    (should (eq (get-text-property 0 'face result)
                'heralds-cyan-face))))

(ert-deftest test-heralds-property-viewnodes-are-friendly-and-colon-free ()
  "Property-viewnode heralds carry the whole titleless viewnode label."
  (dolist (case '((hadId "☮ had ID before import")
                  (wasOverloaded "☮ was overloaded during org-roam import")
                  (noSearchMatching "☮ no search matching")))
    (let ((result (heralds-from-metadata
                   (format "(skg (property %s))" (car case)))))
      (should (equal (substring-no-properties result) (cadr case)))
      (should-not (string-match-p ":" result))
      (should (eq (get-text-property 0 'face result)
                  'heralds-green-face)))))

(ert-deftest test-heralds-property-request-displays-one-semantic-state ()
  "A property edit request is one state change, not one herald per argument."
  (dolist (case '((true  "request:no search matching")
                  (false "request:search matching")))
    (let ((result
           (heralds-from-metadata
            (format
             "(skg (node (id 1) (editRequest (property noSearchMatching %s))))"
             (car case)))))
      (should (equal (substring-no-properties result) (cadr case)))
      (should (eq (get-text-property 0 'face result)
                  'heralds-red-face)))))

(ert-deftest test-heralds-scaffold-display ()
  "Test that scaffold kinds are displayed correctly."
  (with-temp-buffer
    ;; Test aliasFolder
    (erase-buffer)
    (insert "(skg aliasFolder)")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "aliases" result)))

    ;; Test alias
    (erase-buffer)
    (insert "(skg alias)")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "alias" result)))

    ;; Test aliasFolder with folded
    (erase-buffer)
    (insert "(skg folded aliasFolder)")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "aliases" result)))

    ;; Test textChanged with both stages.
    ;; The rule has a string-literal prefix "text changed : " that
    ;; should ride along with each of the sub-rule outputs.
    (erase-buffer)
    (insert "(skg (textChanged staged unstaged))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "text changed : staged" result))
      (should (string-match "text changed : unstaged" result)))))

(ert-deftest test-heralds-diff-display ()
  "Test that staged/unstaged axes are displayed as staged:.../unstaged:... heralds."
  (with-temp-buffer
    ;; ActiveNode with unstaged membership removal (the v.1 'removed-here').
    (erase-buffer)
    (insert "(skg (node (id 1) (source s) (unstaged removedM)))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "unstaged" result))
      (should (string-match "M" result)))

    ;; ActiveNode with unstaged file creation + membership add (the v.1 'new').
    (erase-buffer)
    (insert "(skg (node (id 2) (source s) (unstaged newX newM)))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "unstaged" result))
      (should (string-match "X" result))
      (should (string-match "M" result)))

    ;; Scaffold alias with staged membership add.
    (erase-buffer)
    (insert "(skg alias (staged newM))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "staged:M" result)))

    ;; Scaffold alias with unstaged membership removal.
    (erase-buffer)
    (insert "(skg alias (unstaged removedM))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "unstaged" result))
      (should (string-match "-M" result)))))

(ert-deftest test-heralds-inactive-node-display ()
  "An anonymous inactive-node placeholder displays as a blue herald.
The server emits the bare atom `inactiveNode' (like the other
dataless scaffold markers) -- it carries no id/source, because those
would leak content the user hid by restricting the source-set."
  (with-temp-buffer
    (insert "(skg inactiveNode)")
    (let ((result (heralds-from-metadata
                   "(skg inactiveNode)")))
      (should (equal (substring-no-properties result)
                     "node from inactive source"))
      (should (eq (get-text-property 0 'face result)
                  'heralds-blue-face)))
    (heralds-minor-mode 1)
    (let* ((display-overlay
            (cl-find-if (lambda (ov) (overlay-get ov 'display))
                        (overlays-at (point-min))))
           (display-text (overlay-get display-overlay 'display)))
      (should display-overlay)
      (should (equal (substring-no-properties display-text)
                     "node from inactive source"))
      (should (eq (get-text-property 0 'face display-text)
                  'heralds-blue-face)))))

(ert-deftest test-heralds-survive-major-mode-switch ()
  "After a major-mode switch orphans overlays, disabling heralds
should still remove them."
  (with-temp-buffer
    (insert "(skg (node (id 1) (source s) (rels (contains (out 2)))))\n")
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
        (insert "(skg (node (id 1) (source s) (rels (contains (out 2)))))\n")
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
        (insert "(skg (node (id 1)))\n")
        (heralds-minor-mode 1)
        (should-not heralds-minor-mode)     ;; turned itself off
        (should-not heralds--transform-rules)
        (should (= calls skg-herald-rules-max-attempts)) ;; tried exactly N times
        (should-not (cl-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-in (point-min) (point-max))))))))

(provide 'test-heralds-minor-mode)
