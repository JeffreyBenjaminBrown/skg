(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'heralds-minor-mode)
(require 'skg-reload)
(require 'skg-request-reload-paths)

(ert-deftest test-skg-reload-preserves-herald-rules-on-load-error ()
  "A load error mid-reload must NOT strip the herald rule table.

Regression guard for a volatile-session-state bug: the herald rule
table is fetched once per connect (`skg-request-herald-rules') and has
no on-disk source.  `skg-reload' unloads `heralds-minor-mode' -- which
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
error AND leave the captured table installed."
  (let ((heralds--transform-rules '(skg test-sentinel)))
    (cl-letf (((symbol-function 'skg--reload-modules)
               (lambda (&rest _)
                 (setq heralds--transform-rules nil)
                 (error "simulated load error during reload"))))
      (should-error (skg-reload))
      (should (equal heralds--transform-rules '(skg test-sentinel))))))

(ert-deftest test-skg-reload-path-scan-uses-only-direct-regular-files ()
  "Nested .skg files and .skg-named directories are outside a source."
  (let* ((root (make-temp-file "skg-reload-paths" t))
         (source (expand-file-name "owned/source" root))
         (nested (expand-file-name "nested" source))
         (config (expand-file-name "skgconfig.toml" root))
         (skg-config-dir (file-name-as-directory root)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name "directory.skg" source))
          (with-temp-file config
            (insert "[[sources]]\npath = \"owned/source\"\n"))
          (with-temp-file (expand-file-name "direct.skg" source)
            (insert "pid: direct\n"))
          (with-temp-file (expand-file-name "nested.skg" nested)
            (insert "pid: nested\n"))
          (should (equal (skg--reload-all-skg-files)
                         (list (expand-file-name "direct.skg" source)))))
      (delete-directory root t))))

(provide 'test-skg-reload)
