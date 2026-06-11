(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'heralds-minor-mode)
(skg-test-install-herald-rules)

(ert-deftest test-heralds-minor-mode-toggle ()
  "Test that heralds-minor-mode properly adds and removes overlays."
  (with-temp-buffer
    (progn ;; Insert test text with herald markers
      (insert "Test line with (skg (node (id 123) (graphStats (contents 2) (containsHerald (contents 2))) (viewStats cycle))) herald\n")
      (insert "Another line (skg (node (id 456) (graphStats (linksInFromContainers 3) (linksHerald 3→)) (editRequest delete))) more text\n")
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
  "Test that the display property is properly set and cleared."
  (with-temp-buffer
    (insert "Line with (skg (node (id 123) (parentIs independent) (birth linksToParent) (graphStats (containers 3) (containsHerald (containers 3)) (linksInFromLeaves 1) (linksHerald →1)) (viewStats cycle) (editRequest delete))) text")
    (progn ;; what happens upon enabling heralds-minor-mode
      (heralds-minor-mode 1)
      (let* ;; Find the overlay covering our herald
          ( ( herald-start
              ( save-excursion
                ( goto-char ( point-min ))
                ( search-forward "(skg " )
                ( match-beginning 0 )) )
            ( overlays-at-herald ( overlays-at herald-start ))
            ( display-overlay
              ( cl-find-if ( lambda ( ov ) ( overlay-get ov 'display ))
                           overlays-at-herald )) )
        (should display-overlay) ;; Should have a display overlay
        (should ( stringp ( overlay-get display-overlay 'display )) )
        (let ;; The display should contain our herald symbols
            ( ( display-text ( overlay-get display-overlay 'display )) )
          ( should ( string-match "←" display-text ))
          ( should ( string-match "⟳" display-text ))
          ( should ( string-match "3{" display-text ))
          ( should ( string-match "→1" display-text ))
          ( should (< (string-match "→1" display-text)
                      (string-match "3{" display-text)) )
          ( should ( string-match "delete" display-text )) )) )
    (progn ;; what happens upon disabling it
      (heralds-minor-mode -1) ;; disable
      (let* ;; Check that no display overlays remain
          ( ( herald-start
              ( save-excursion
                ( goto-char ( point-min ))
                ( search-forward "(skg " )
                ( match-beginning 0 )) )
            ( overlays-at-herald ( overlays-at herald-start ))
            ( display-overlay
              ( cl-find-if ( lambda ( ov ) ( overlay-get ov 'display ))
                           overlays-at-herald )) )
        ( should-not display-overlay )) )) )

(ert-deftest test-heralds-viewrequests-display ()
  "Test that viewRequests are displayed as req:* heralds."
  (with-temp-buffer
    ;; Test aliases viewRequest
    (erase-buffer)
    (insert "(skg (node (id 1) (viewRequests aliases)))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:aliases" result)))

    ;; Test containerwardView viewRequest
    (erase-buffer)
    (insert "(skg (node (id 2) (viewRequests containerwardView)))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:containers" result)))

    ;; Test sourcewardView viewRequest
    (erase-buffer)
    (insert "(skg (node (id 3) (viewRequests sourcewardView)))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:sources" result)))

    ;; Test multiple viewRequests
    (erase-buffer)
    (insert "(skg (node (id 4) (viewRequests aliases containerwardView sourcewardView)))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "req:aliases" result))
      (should (string-match "req:containers" result))
      (should (string-match "req:sources" result)))))

(ert-deftest test-heralds-scaffold-display ()
  "Test that scaffold kinds are displayed correctly."
  (with-temp-buffer
    ;; Test aliasCol
    (erase-buffer)
    (insert "(skg aliasCol)")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "aliases" result)))

    ;; Test alias
    (erase-buffer)
    (insert "(skg alias)")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "alias" result)))

    ;; Test aliasCol with folded
    (erase-buffer)
    (insert "(skg folded aliasCol)")
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
    ;; TrueNode with unstaged membership removal (the v.1 'removed-here').
    (erase-buffer)
    (insert "(skg (node (id 1) (source s) (unstaged removedM)))")
    (let ((result (heralds-from-metadata (buffer-string))))
      (should (string-match "unstaged" result))
      (should (string-match "M" result)))

    ;; TrueNode with unstaged file creation + membership add (the v.1 'new').
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
  "Inactive-node metadata should be masked by a blue placeholder herald."
  (with-temp-buffer
    (insert "(skg (inactiveNode (id hidden) (source private)))")
    (let ((result (heralds-from-metadata
                   "(skg (inactiveNode (id hidden) (source private)))")))
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
    (insert "(skg (node (id 1) (source s) (graphStats (contents 2) (containsHerald (contents 2)))))\n")
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

(provide 'test-heralds-minor-mode)
