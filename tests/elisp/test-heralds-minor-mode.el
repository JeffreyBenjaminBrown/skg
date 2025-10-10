(require 'ert)
(require 'heralds-minor-mode)

(ert-deftest test-heralds-minor-mode-toggle ()
  "Test that heralds-minor-mode properly adds and removes overlays."
  (with-temp-buffer
    (progn ;; Insert test text with herald markers
      (insert "Test line with (skg id:123 repeated type:aliases) herald\n")
      (insert "Another line (skg id:456 blue:test) more text\n")
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
    (insert "Line with (skg id:123 repeated) text")
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
          ( should ( string-match "⅄" display-text ))
          ( should ( string-match "REP" display-text )) )) )
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

(provide 'test-heralds-minor-mode)
