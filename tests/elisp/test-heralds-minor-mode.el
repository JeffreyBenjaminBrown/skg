(require 'ert)
(add-to-list
 'load-path
 (expand-file-name "../../elisp"
                   (file-name-directory load-file-name)))
(add-to-list
 'load-path
 (expand-file-name "../../elisp/skg-sexpr"
                   (file-name-directory load-file-name)))
(require 'heralds-minor-mode)

(ert-deftest test-heralds-minor-mode-toggle ()
  "Test that heralds-minor-mode properly adds and removes overlays."
  (with-temp-buffer
    (progn ;; Insert test text with herald markers
      (insert "Test line with (skg (node (id 123) cycle (stats (contents 2)))) herald\n")
      (insert "Another line (skg (node (id 456) (stats (containers 1) (linksIn 3)) (editRequest delete))) more text\n")
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
    (insert "Line with (skg (node (id 123) cycle (stats notInParent (containers 3) (linksIn 1)) (editRequest delete))) text")
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
          ( should ( string-match "⟳" display-text ))
          ( should ( string-match "!{" display-text ))
          ( should ( string-match "3{" display-text ))
          ( should ( string-match "1→" display-text ))
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
      (should (string-match "aliases" result)))))

(provide 'test-heralds-minor-mode)
