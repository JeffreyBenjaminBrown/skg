(require 'ert)
(require 'cl-lib)
(require 'heralds-minor-mode)
(require 'skg-state)
(require 'skg-reload)

(ert-deftest test-skg-reload-preserves-herald-rules-on-load-error ()
  "A load error mid-reload must not strip the herald rule table.

Regression guard for a volatile-session-state bug: the herald rule
table is fetched once per connect (`skg-request-herald-rules') and has
no on-disk repo.  `skg-reload' unloads `heralds-minor-mode' -- which
wipes the table -- and is supposed to re-install the captured copy
afterward.  When the re-install was a plain sequential step, any error
in a reloaded file (a stray edit-in-progress, say) aborted `skg-reload'
before that step, leaving the table nil for the rest of the session;
every `heralds-minor-mode' toggle then reported \"no herald rule table
from the server\" even though the server was healthy.

The fix wraps the re-install in `unwind-protect'.  Here we stub the
destructive `skg--reload-modules' so the real reload machinery never
runs (no feature is actually unloaded); the stub mimics the failure by
wiping the table and signalling.  `skg-reload' must still surface the
error and leave the captured table installed."
  (let ((heralds--transform-rules '(skg test-sentinel))
        (heralds--skgrepo-labels '(("main" . "m"))))
    (cl-letf (((symbol-function 'skg--reload-modules)
               (lambda (&rest _)
                 (setq heralds--transform-rules nil
                       heralds--skgrepo-labels nil)
                 (error "simulated load error during reload"))))
      (should-error (skg-reload))
      (should (equal heralds--transform-rules '(skg test-sentinel)))
      (should (equal heralds--skgrepo-labels '(("main" . "m")))))))

(ert-deftest test-skg-reload-preserves-client-settings ()
  "`skg-reload' must keep what `skg-client-init' set, and the linkstack.

Regression guard: `skg-reload' unloads `skg-state', which wiped
`skg-port', `skg-config-file-path' and `skg-config-dir'.  The next
request then failed with \"Wrong type argument: stringp, nil\" (no port
to reconnect to) until the user ran `skg-client-init' again.  It also
wiped the user's `skg-linkstack'.

As above, `skg--reload-modules' is stubbed so nothing is actually
unloaded; the stub wipes the variables the way `unload-feature' does.
Checked both when the reload succeeds and when a reloaded file
signals."
  (dolist (fails '(nil t))
    (let ((skg-port "1999")
          (skg-config-file-path "/x/y.toml")
          (skg-config-dir "/x/")
          (skg-linkstack '(("id1" "title 1")))
          (heralds--transform-rules nil))
      (cl-letf (((symbol-function 'skg--reload-modules)
                 (lambda (&rest _)
                   (setq skg-port nil
                         skg-config-file-path nil
                         skg-config-dir nil
                         skg-linkstack nil)
                   (when fails
                     (error "simulated load error during reload")))))
        (if fails
            (should-error (skg-reload))
          (skg-reload))
        (should (equal skg-port "1999"))
        (should (equal skg-config-file-path "/x/y.toml"))
        (should (equal skg-config-dir "/x/"))
        (should (equal skg-linkstack '(("id1" "title 1"))))))))

(provide 'test-skg-reload)
