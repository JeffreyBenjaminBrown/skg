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

(ert-deftest test-heralds-minor-mode-toggle ()
  "Test that heralds-minor-mode properly adds and removes overlays."
  (with-temp-buffer
    (progn ;; Insert test text with herald markers
      (insert "Test line with (skg (node (id 123) (rels (contains (out 2))) (viewStats cycle))) herald\n")
      (insert "Another line (skg (node (id 456) (rels (links_to (in 3 (substantive 3)))) (editRequest delete))) more text\n")
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
the C token 2aC: the in-side numeral \"2\" (medium), the ancestor \"a\"
at its floor (low), and the birth letter \"C\" (high)."
  (with-temp-buffer
    (insert "Line with (skg (node (id 123) (affectsParent false) (rels (contains (in 2 (ancestors 1))) (birth (contains in 1))) (viewStats cycle) (editRequest delete))) text")
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
                          'heralds-high-face )) )))) ;; the "C"
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

(ert-deftest test-heralds-inactive-node-display ()
  "An anonymous inactive-node placeholder displays as a message herald.
The server emits the bare atom `inactiveNode' (like the other
dataless non-vognode markers) -- it carries no id/repo, because those
would leak content the user hid by restricting the repo-set."
  (with-temp-buffer
    (insert "(skg inactiveNode)")
    (let ((result (heralds-from-metadata
                   "(skg inactiveNode)")))
      (should (equal (substring-no-properties result)
                     "node from inactive repo"))
      (should (eq (get-text-property 0 'face result)
                  'heralds-message-face)))
    (heralds-minor-mode 1)
    (let* ((display-overlay
            (cl-find-if (lambda (ov) (overlay-get ov 'display))
                        (overlays-at (point-min))))
           (display-text (overlay-get display-overlay 'display)))
      (should display-overlay)
      (should (equal (substring-no-properties display-text)
                     "node from inactive repo"))
      (should (eq (get-text-property 0 'face display-text)
                  'heralds-message-face)))))

(ert-deftest test-heralds-survive-major-mode-switch ()
  "After a major-mode switch orphans overlays, disabling heralds
should still remove them."
  (with-temp-buffer
    (insert "(skg (node (id 1) (repo s) (rels (contains (out 2)))))\n")
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
        (insert "(skg (node (id 1) (repo s) (rels (contains (out 2)))))\n")
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
