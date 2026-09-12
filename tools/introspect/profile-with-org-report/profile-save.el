;;; profile-save.el --- Drive the recursive-content save profile -*- lexical-binding: t; -*-

(require 'profiler)
(load-file (expand-file-name "../../../elisp/skg-init.el"
                             (file-name-directory load-file-name)))

(defvar skg-port)
(defvar skg-profile-save-finished nil)

(defun skg-profile--write-signal-file (environment-name)
  (let ((path (getenv environment-name)))
    (when path
      (write-region "" nil path nil 'silent))))

(defun skg-profile--wait-until (predicate description)
  (let ((deadline (+ (float-time) 180.0)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (funcall predicate)
      (error "Timed out waiting for %s" description))))

(defun skg-profile--view-buffer (root-pid)
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (boundp 'skg-view-uri)
            skg-view-uri
            (save-excursion
              (goto-char (point-min))
              (search-forward root-pid nil t)))))
   (buffer-list)))

(defun skg-profile--count-headlines ()
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^\\*+ " nil t)
        (setq count (1+ count)))
      count)))

(defun skg-profile--server-cpu-seconds ()
  (let* ((pid (getenv "PROFILE_SERVER_PID"))
         (clock-ticks (string-to-number (getenv "PROFILE_CLOCK_TICKS")))
         (stat-path (format "/proc/%s/stat" pid))
         (stat (with-temp-buffer
                 (insert-file-contents stat-path)
                 (buffer-string)))
         (after-command (substring stat (1+ (string-match ") " stat))))
         (fields (split-string after-command " " t))
         (user-ticks (string-to-number (nth 11 fields)))
         (system-ticks (string-to-number (nth 12 fields))))
    (/ (+ user-ticks system-ticks) (float clock-ticks))))

(defun skg-profile--save-result-arrived (&rest _arguments)
  (setq skg-profile-save-finished t))

(defun skg-profile--append-measurement
    (run start-epoch end-epoch wall-seconds server-cpu-seconds
         emacs-cpu-seconds bytes-before headlines-before
         bytes-after headlines-after)
  (let ((path (expand-file-name "client.tsv" (getenv "PROFILE_RAW_DIR"))))
    (write-region
     (format "%d\t%.9f\t%.9f\t%.9f\t%.9f\t%.9f\t%d\t%d\t%d\t%d\n"
             run start-epoch end-epoch wall-seconds server-cpu-seconds
             emacs-cpu-seconds bytes-before headlines-before
             bytes-after headlines-after)
     nil path t 'silent)))

(defun skg-profile--write-emacs-profile ()
  (let* ((raw-dir (getenv "PROFILE_RAW_DIR"))
         (data-path (expand-file-name "emacs-profile.data" raw-dir))
         (text-path (expand-file-name "emacs-profile.txt" raw-dir))
         (profile (profiler-cpu-profile))
         (report-buffer (profiler-report-setup-buffer profile)))
    (profiler-write-profile profile data-path)
    (with-current-buffer report-buffer
      (write-region (point-min) (point-max) text-path nil 'silent))))

(defun skg-profile--run ()
  (let* ((skg-port (string-to-number (getenv "PROFILE_PORT")))
         (root-pid (getenv "PROFILE_ROOT_PID"))
         (runs (string-to-number (getenv "PROFILE_RUNS")))
         (between-run-seconds
          (string-to-number (or (getenv "PROFILE_BETWEEN_RUN_SECONDS") "0")))
         (client-tsv (expand-file-name "client.tsv" (getenv "PROFILE_RAW_DIR"))))
    (setenv "SKG_PORT" (number-to-string skg-port))
    (setq skg-port skg-port)
    (write-region
     "run\tstart_epoch\tend_epoch\twall_seconds\tserver_cpu_seconds\temacs_cpu_seconds\tbytes_before\theadlines_before\tbytes_after\theadlines_after\n"
     nil client-tsv nil 'silent)
    (advice-add 'skg--save-result-handler :after
                #'skg-profile--save-result-arrived)
    (skg-request-single-root-content-view-from-id root-pid)
    (skg-profile--wait-until
     (lambda () (skg-profile--view-buffer root-pid)) "recursive content view")
    (let ((view-buffer (skg-profile--view-buffer root-pid)))
      (unless view-buffer
        (error "The requested recursive content view was not created"))
      (skg-profile--write-signal-file "PROFILE_READY_FILE")
      (skg-profile--wait-until
       (lambda () (file-exists-p (getenv "PROFILE_GO_FILE")))
       "native profiler attachment")
      (profiler-start 'cpu)
      (dotimes (zero-based-run runs)
        (let ((run (1+ zero-based-run)))
          (with-current-buffer view-buffer
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "** profile%d\n" run))
            (let ((bytes-before (string-bytes (buffer-string)))
                  (headlines-before (skg-profile--count-headlines)))
              (setq skg-profile-save-finished nil)
              (let ((wall-start (float-time))
                    (server-cpu-start (skg-profile--server-cpu-seconds))
                    (cpu-start (float-time (get-internal-run-time))))
                (skg-request-save-buffer)
                (skg-profile--wait-until
                 (lambda () skg-profile-save-finished) "save response")
                (let ((wall-end (float-time)))
                  (skg-profile--append-measurement
                 run
                 wall-start
                 wall-end
                 (- wall-end wall-start)
                 (- (skg-profile--server-cpu-seconds) server-cpu-start)
                 (- (float-time (get-internal-run-time)) cpu-start)
                 bytes-before headlines-before
                 (string-bytes (buffer-string))
                 (skg-profile--count-headlines))))))
          (when (< (1+ zero-based-run) runs)
            (sleep-for between-run-seconds))))
      (profiler-stop)
      (skg-profile--write-emacs-profile)
      (skg-profile--write-signal-file "PROFILE_DONE_FILE"))))

(condition-case error-data
    (progn
      (skg-profile--run)
      (kill-emacs 0))
  (error
   (message "SAVE PROFILE FAILED: %S" error-data)
   (kill-emacs 1)))

;;; profile-save.el ends here
