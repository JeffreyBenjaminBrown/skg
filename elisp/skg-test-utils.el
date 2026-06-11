;;; -*- lexical-binding: t; -*-
;;;
;;; Utilities for skg tests (unit and integration).
;;;
;;; For unit tests in tests/elisp/, load this file first to set up load-path:
;;;   (load-file "../../elisp/skg-test-utils.el")
;;;   (require 'ert)
;;;   (require 'your-module-here)

(require 'cl-lib)

;; Set up load-path for test files.
;; This adds both elisp/ and elisp/skg-sexpr/ directories.
(defvar skg-test--project-root nil
  "Repo root, captured when skg-test-utils.el is loaded.")
(when load-file-name
  (let ((elisp-dir (file-name-directory load-file-name)))
    (setq skg-test--project-root
          (file-name-directory (directory-file-name elisp-dir)))
    (add-to-list 'load-path elisp-dir)
    (add-to-list 'load-path (expand-file-name "skg-sexpr" elisp-dir))
    (add-to-list 'load-path (expand-file-name "skg-sexpr-edit" elisp-dir))
    (add-to-list 'load-path (expand-file-name "skg-request-save" elisp-dir))))

(defun skg-test-install-herald-rules ()
  "Install the herald rule table from the generated fixture.
In production the table is fetched from the server at connect time
\(it lives only in Rust, server/heralds.rs); batch-mode tests inject
it from tests/elisp/herald-rules.sexp instead. That fixture is
pinned to the live table by the Rust test
`elisp_fixture_matches_the_live_table', so it cannot go stale
silently; regenerate it with
  cargo run --bin emit-herald-rules > tests/elisp/herald-rules.sexp"
  (require 'heralds-minor-mode)
  (let ((fixture (expand-file-name "tests/elisp/herald-rules.sexp"
                                   skg-test--project-root)))
    (with-temp-buffer
      (insert-file-contents fixture)
      (heralds-install-rules
       (car (read-from-string (buffer-string)))))))

(defun find-skg-content-buffer ()
  "Find a skg content view buffer that's not a search buffer."
  (car (cl-remove-if-not
        (lambda (b)
          (and (with-current-buffer b
                 (derived-mode-p 'skg-content-view-mode))
               (not (string-prefix-p "*?" (buffer-name b)))))
        (buffer-list))))

(provide 'skg-test-utils)
