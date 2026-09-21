;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Detached, lossless recovery documents for dirty skg views.

(require 'json)
(require 'skg-buffer)

(defun skg-show-unsaved-changes (path &optional overwrite)
  "Archive this view's baseline and current text in detached Org at PATH.
With OVERWRITE non-nil, replace an existing PATH without prompting."
  (interactive
   (list (read-file-name "Write unsaved-changes recovery document: ") nil))
  (unless (and skg-view-uri (skg-buffer-p (current-buffer)))
    (user-error "This buffer is not a live skg view"))
  (let* ((source-buffer (current-buffer))
         (destination (expand-file-name path))
         (baseline skg-clean-baseline)
         (current (buffer-substring-no-properties (point-min) (point-max)))
         (document
          (skg--unsaved-changes-document baseline current)))
    (when (and (file-exists-p destination)
               (not overwrite)
               (not (yes-or-no-p
                     (format "Overwrite recovery document %s? "
                             destination))))
      (user-error "Recovery document was not written"))
    (with-temp-buffer
      (insert document)
      (write-region (point-min) (point-max) destination nil 'silent))
    (find-file destination)
    (setq-local skg-view-uri nil)
    (setq-local skg-clean-baseline nil)
    (set-buffer-modified-p nil)
    (message "Created detached unsaved-changes recovery document: %s"
             (file-truename destination))
    source-buffer))

(defun skg--unsaved-changes-document (
    baseline current)
  "Return a detached recovery document for BASELINE and CURRENT."
  (let ((baseline-available (stringp baseline)))
    (concat
     "#+TITLE: Skg unsaved changes recovery\n\n"
     "* Recovery instructions\n"
     "This is a detached local archive, not a live Skg view. Close archived "
     "views, save the remaining live view, reopen fresh views, and manually "
     "reapply the intended edits. Do not save this entire stale snapshot over "
     "newer graph data.\n\n"
     "* Context\n"
     (format "- Buffer name: %s\n" (buffer-name))
     (format "- View URI: %s\n" skg-view-uri)
     (format "- Captured UTC: %s\n"
             (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
     (format "- Git diff mode: %s\n"
             (if (plist-get skg-clean-baseline-context :git-diff-mode)
                 "enabled" "disabled or unavailable"))
     "- Source-set: unavailable to this client\n"
     (if skg--search-enrichment-includes-user-edits
         (concat "- Search enrichment: current text includes both unsaved "
                 "edits and enrichment presentation; the diff is not a "
                 "semantic patch of user intent alone.\n")
       "- Search enrichment: no dirty enrichment recorded\n")
     (if baseline-available
         "- Baseline: verified client clean baseline\n"
       (concat "- Baseline: unavailable (the current text is still preserved, "
               "but no verified edit diff can be produced)\n"))
     "\n* Exact snapshots\n"
     "Each source block is one JSON string. JSON decoding reproduces every "
     "character, including tabs and trailing newlines.\n\n"
     "** Clean baseline\n"
     (if baseline-available
         (concat "#+begin_src json\n" (json-encode-string baseline)
                 "\n#+end_src\n")
       "Unavailable.\n")
     "\n** Current text\n#+begin_src json\n"
     (json-encode-string current)
     "\n#+end_src\n\n"
     "* Readable unified diff\n"
     (if baseline-available
         (concat "#+begin_src diff\n"
                 (skg--unified-diff baseline current)
                 "#+end_src\n")
       "Unavailable because the clean baseline was not captured.\n"))))

(defun skg--unified-diff (baseline current)
  "Return a readable unified diff from BASELINE to CURRENT."
  (let ((baseline-file (make-temp-file "skg-recovery-baseline-"))
        (current-file (make-temp-file "skg-recovery-current-")))
    (unwind-protect
        (progn
          (with-temp-file baseline-file (insert baseline))
          (with-temp-file current-file (insert current))
          (with-temp-buffer
            (let ((status
                   (call-process "diff" nil t nil "-u"
                                 "--label" "clean baseline"
                                 "--label" "current text"
                                 baseline-file current-file)))
              (unless (memq status '(0 1))
                (error "Could not create recovery diff (diff exit %s)"
                       status))
              (if (= status 0)
                  "No textual changes.\n"
                (buffer-string)))))
      (delete-file baseline-file)
      (delete-file current-file))))

(provide 'skg-recovery)
