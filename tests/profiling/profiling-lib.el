;;; -*- lexical-binding: t; -*-
;;;
;;; Common profiling framework for skg benchmark tests.
;;; Each test-emacs.el loads skg-init.el, then this file,
;;; defines benchmark-phase-edit, and calls (run-profiling-test).

(require 'cl-lib)

;; Timing results
(defvar benchmark-view-time nil)
(defvar benchmark-edit-time nil)
(defvar benchmark-save-time nil)
(defvar benchmark-completed nil)

;; Flags set by advice to detect response arrival
(defvar benchmark-view-response-arrived nil)
(defvar benchmark-save-response-arrived nil)

;; Advise the content view handler to set a flag on completion.
(advice-add 'skg-handle-content-view-sexp :after
            (lambda (&rest _args)
              (setq benchmark-view-response-arrived t)))

;; Advise the save handler to set a flag on completion.
(advice-add 'skg-handle-save-sexp :after
            (lambda (&rest _args)
              (setq benchmark-save-response-arrived t)))

(defun benchmark-format-time (time-a time-b)
  "Format elapsed time between TIME-A and TIME-B as a string."
  (format "%.3f" (float-time (time-subtract time-b time-a))))

(defun benchmark-poll-until (predicate timeout-seconds interval)
  "Poll PREDICATE every INTERVAL seconds, up to TIMEOUT-SECONDS.
Returns t if predicate became true, nil on timeout."
  (let ((elapsed 0.0))
    (while (and (not (funcall predicate))
                (< elapsed timeout-seconds))
      (accept-process-output nil interval)
      (sleep-for interval)
      (setq elapsed (+ elapsed interval)))
    (funcall predicate)))

(defun benchmark-phase-view ()
  "Phase 1: Request content view and time the round-trip."
  (message "=== PHASE 1: Requesting content view for node '0' ===")
  (setq benchmark-view-response-arrived nil)
  (let ((t1 (current-time)))
    (skg-request-single-root-content-view-from-id "0")
    ;; Poll until the response arrives (up to 120s for large dataset)
    (if (benchmark-poll-until
         (lambda () benchmark-view-response-arrived)
         120.0 0.1)
        (let ((t2 (current-time)))
          (setq benchmark-view-time
                (benchmark-format-time t1 t2))
          (message "emacs_request_to_response: %ss" benchmark-view-time)
          ;; Verify buffer exists
          (let ((buf (get-buffer "*skg: 0*")))
            (if buf
                (progn
                  (message "View buffer created with %d characters"
                           (with-current-buffer buf (buffer-size)))
                  (benchmark-phase-edit))
              (progn
                (message "FAIL: View buffer *skg: 0* not created")
                (kill-emacs 1)))))
      (progn
        (message "TIMEOUT: View response not received within 120s")
        (kill-emacs 1)))))

(defun benchmark-phase-save ()
  "Phase 3: Save buffer and time the round-trip."
  (message "=== PHASE 3: Saving buffer ===")
  (setq benchmark-save-response-arrived nil)
  (with-current-buffer "*skg: 0*"
    (let ((t5 (current-time)))
      (skg-request-save-buffer)
      ;; Poll until save response arrives (up to 300s for large dataset)
      (if (benchmark-poll-until
           (lambda () benchmark-save-response-arrived)
           300.0 0.1)
          (let ((t6 (current-time)))
            (setq benchmark-save-time
                  (benchmark-format-time t5 t6))
            (message "emacs_save_round_trip: %ss" benchmark-save-time)
            (benchmark-report-and-exit))
        (progn
          (message "TIMEOUT: Save response not received within 300s")
          (kill-emacs 1))))))

(defun benchmark-report-and-exit ()
  "Print final timing report and exit."
  (message "")
  (message "=== Benchmark Results (Emacs-side) ===")
  (message "emacs_request_to_response: %ss" benchmark-view-time)
  (message "emacs_edit: %ss" benchmark-edit-time)
  (message "emacs_save_round_trip: %ss" benchmark-save-time)
  (message "=== Benchmark Complete ===")
  (setq benchmark-completed t)
  (kill-emacs 0))

(defun run-profiling-test ()
  "Main benchmark orchestrator."
  (message "=== SKG Benchmark Test ===")
  ;; Set port from environment
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (sleep-for 0.25)
  ;; Global timeout: 10 minutes
  (run-at-time
   600 nil
   (lambda ()
     (message "TIMEOUT: Benchmark timed out after 10 minutes!")
     (kill-emacs 1)))
  ;; Phase 1: Request view
  (benchmark-phase-view))
