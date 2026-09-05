;;; skg-worktree-guard.el --- Keep dirty views safe from worktree writes  -*- lexical-binding: t; -*-

;;; Commentary:
;; A raw .skg save or a Magit command that writes a configured source can
;; provoke a disk reload.  Until every dirty view has been saved, Skg cannot
;; know how to combine those disk bytes with the user's unsaved buffer edits.
;; Refuse that client-side mutation instead of manufacturing the conflict.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer)
(require 'skg-config)

(defvar-local skg--raw-file-recorded-disk-state nil
  "Exact disk state from the last raw-file read or successful save.")
(put 'skg--raw-file-recorded-disk-state 'permanent-local t)

(defvar-local skg--raw-file-externally-stale nil
  "Non-nil after the raw file's live bytes differ from its recorded state.")
(put 'skg--raw-file-externally-stale 'permanent-local t)

(defun skg--raw-file-current-disk-state (&optional path)
  "Return exact safe state of PATH as (regular SHA256) or (absent).
Every other filesystem type or inspection failure returns (unsafe REASON)."
  (let ((path (or path buffer-file-name)))
    (condition-case error-data
        (cond
         ((not (stringp path)) '(unsafe "buffer has no file name"))
         ((file-symlink-p path) '(unsafe "path is a symbolic link"))
         ((not (file-exists-p path)) '(absent))
         ((not (file-regular-p path))
          '(unsafe "path is not a regular file"))
         (t
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (list 'regular (secure-hash 'sha256 (current-buffer))))))
      (file-error
       (list 'unsafe (error-message-string error-data))))))

(defun skg-record-raw-file-disk-state (&optional buffer)
  "Record BUFFER's exact raw-file disk baseline after a read or save."
  (with-current-buffer (or buffer (current-buffer))
    (setq skg--raw-file-recorded-disk-state
          (skg--raw-file-current-disk-state buffer-file-name)
          skg--raw-file-externally-stale nil)
    (when (and skg--buffer-record
               (eq (skg--buffer-record-kind skg--buffer-record)
                   'raw-skg-file))
      (let ((text (skg-buffer-raw-text)))
        (setf (skg--buffer-record-last-fetched skg--buffer-record) text
              (skg--buffer-record-last-fetched-sha256 skg--buffer-record)
              (skg--sha256-text text))))
    skg--raw-file-recorded-disk-state))

(defun skg--raw-file-safe-disk-state-p (state)
  "Whether STATE can serve as an exact raw-file before fact."
  (memq (car-safe state) '(regular absent)))

(defun skg--describe-raw-file-disk-state (state)
  "Return a concise user-facing description of raw-file STATE."
  (pcase (car-safe state)
    ('regular (format "SHA-256 %s" (cadr state)))
    ('absent "absence")
    ('unsafe (format "unsafe state (%s)" (or (cadr state) "unknown")))
    (_ "no recorded state")))

(defun skg--queue-raw-file-observation ()
  "Queue a nonmutating exact sweep after a raw-file event."
  (if (fboundp 'skg--request-reload-full-sweep)
      (condition-case error-data
          (skg--request-reload-full-sweep)
        (error
         (display-warning
          'skg
          (format "Raw .skg change is safe, but observation could not be queued: %s"
                  (error-message-string error-data))
          :warning)))
    (display-warning
     'skg
     "Raw .skg change is safe, but the observation command is not loaded"
     :warning)))

(defun skg--dirty-view-buffers ()
  "Return every live Skg view with unsaved changes."
  (cl-remove-if-not
   (lambda (buffer)
     (and (skg-buffer-p buffer)
          (skg-buffer-dirty-p buffer)))
   (buffer-list)))

(defun skg--dirty-view-buffer-names ()
  "Return the names of all dirty Skg views, in display order."
  (mapcar #'buffer-name (skg--dirty-view-buffers)))

(defun skg--refuse-worktree-write-if-views-dirty (&optional action)
  "Refuse ACTION when an Skg view contains unsaved changes."
  (when-let ((names (skg--dirty-view-buffer-names)))
    (user-error
     "%s refused: save or close these Skg views first: %s"
     (or action "Worktree write")
     (mapconcat #'identity names ", "))))

(defun skg--guard-raw-skg-save ()
  "Before-save guard for a raw file in a configured Skg source."
  (when (and buffer-file-name
             (skg--configured-skg-file-p buffer-file-name))
    (unless (and skg--buffer-record
                 (eq (skg--buffer-record-kind skg--buffer-record)
                     'raw-skg-file))
      (when (fboundp 'skg-register-raw-file-buffer-if-configured)
        (skg-register-raw-file-buffer-if-configured (current-buffer))))
    (when (and skg--buffer-record
               (skg--buffer-record-maintenance-epoch skg--buffer-record))
      (user-error
       "Raw .skg save refused: maintenance epoch %s is active"
       (skg--buffer-record-maintenance-epoch skg--buffer-record)))
    (let ((expected skg--raw-file-recorded-disk-state)
          (actual (skg--raw-file-current-disk-state buffer-file-name)))
      (unless (and (skg--raw-file-safe-disk-state-p expected)
                   (skg--raw-file-safe-disk-state-p actual)
                   (equal expected actual))
        (setq skg--raw-file-externally-stale t)
        (skg--queue-raw-file-observation)
        (user-error
         "Raw .skg save refused: disk changed since this buffer was read (expected %s, found %s); revert or reconcile explicitly"
         (skg--describe-raw-file-disk-state expected)
         (skg--describe-raw-file-disk-state actual))))
    (skg--refuse-worktree-write-if-views-dirty "Raw .skg save")))

(defun skg--raw-skg-after-save ()
  "Advance the raw-file baseline and queue exact disk observation."
  (when (and buffer-file-name
             (skg--configured-skg-file-p buffer-file-name))
    (skg-record-raw-file-disk-state (current-buffer))
    (skg--queue-raw-file-observation)))

(defun skg--configured-skg-file-p (path)
  "Whether PATH is a direct .skg child of a configured source."
  (and (stringp path)
       (string-match-p "\\.skg\\'" path)
       (cl-some
        (lambda (source)
          (equal (file-name-as-directory
                  (expand-file-name (file-name-directory path)))
                 (file-name-as-directory
                  (expand-file-name (cdr source)))))
        (skg--source-paths))))

(defun skg--path-at-or-below-p (path directory)
  "Whether PATH is DIRECTORY itself or is below DIRECTORY."
  (let ((path (file-name-as-directory (expand-file-name path)))
        (directory (file-name-as-directory (expand-file-name directory))))
    (or (equal path directory)
        (file-in-directory-p path directory))))

(defun skg--magit-repository-contains-source-p ()
  "Whether the current Magit repository contains a configured source."
  (when-let ((root (and (fboundp 'magit-toplevel)
                        (ignore-errors (magit-toplevel)))))
    (cl-some (lambda (source)
               (skg--path-at-or-below-p (cdr source) root))
             (skg--source-paths))))

(defconst skg--git-read-only-commands
  '("annotate" "blame" "cat-file" "check-attr" "check-ignore"
    "count-objects" "describe" "diff" "diff-files" "diff-index"
    "diff-tree" "for-each-ref" "fsck" "grep" "help" "log"
    "ls-files" "ls-remote" "ls-tree" "merge-base" "name-rev"
    "rev-list" "rev-parse" "shortlog" "show" "show-branch"
    "show-ref" "status" "version" "whatchanged")
  "Git subcommands that do not change the worktree.")

(defconst skg--git-index-or-ref-only-commands
  '("add" "branch" "commit" "config" "fetch" "notes" "push"
    "reflog" "remote" "symbolic-ref" "tag" "update-index"
    "update-ref")
  "Git subcommands allowed because they change only indexes or refs.")

(defun skg--flatten-git-args (args)
  "Flatten Magit's possibly nested ARGS and retain string arguments."
  (cl-remove-if-not #'stringp (flatten-tree args)))

(defun skg--git-option-p (arg)
  "Whether ARG looks like a Git option rather than a subcommand."
  (string-prefix-p "-" arg))

(defun skg--git-command-and-args (args)
  "Return (COMMAND . REST) from Magit's ARGS.
Skip global options conservatively.  Magit normally supplies COMMAND first."
  (let ((flat (skg--flatten-git-args args)))
    (while (and flat (skg--git-option-p (car flat)))
      (setq flat (cdr flat)))
    (cons (car flat) (cdr flat))))

(defun skg--git-command-preserves-worktree-p (args)
  "Whether Git ARGS are provably unable to write worktree files.
Unknown commands return nil: this predicate is a safety boundary, not a
complete model of Git."
  (pcase-let* ((`(,command . ,rest) (skg--git-command-and-args args)))
    (cond
     ((null command) nil)
     ((member command skg--git-read-only-commands) t)
     ((member command skg--git-index-or-ref-only-commands) t)
     ((equal command "reset")
      (not (cl-some (lambda (arg)
                      (member arg '("--hard" "--merge" "--keep")))
                    rest)))
     ((equal command "restore")
      (and (member "--staged" rest)
           (not (member "--worktree" rest))))
     ((equal command "rm") (member "--cached" rest))
     ((equal command "apply")
      (or (member "--cached" rest) (member "--check" rest)))
     ((equal command "read-tree")
      (not (cl-some (lambda (arg)
                      (or (equal arg "-u")
                          (string-prefix-p "--update" arg)))
                    rest)))
     ((equal command "clean")
      (cl-some (lambda (arg)
                 (or (equal arg "-n") (equal arg "--dry-run")))
               rest))
     (t nil))))

(defun skg--guard-magit-git-args (args)
  "Refuse unsafe Magit Git ARGS when dirty Skg views are at risk."
  (when (and (skg--dirty-view-buffers)
             (skg--magit-repository-contains-source-p)
             (not (skg--git-command-preserves-worktree-p args)))
    (let ((command (or (car (skg--git-command-and-args args)) "unknown Git")))
      (skg--refuse-worktree-write-if-views-dirty
       (format "Magit `%s' worktree operation" command)))))

(defun skg--guard-magit-call-git (&rest args)
  "Advice for synchronous `magit-call-git'."
  (skg--guard-magit-git-args args))

(defun skg--guard-magit-start-git (_input &rest args)
  "Advice for asynchronous `magit-start-git'."
  (skg--guard-magit-git-args args))

(defun skg--install-magit-worktree-guards ()
  "Install the two central Magit process guards exactly once."
  (unless (advice-member-p #'skg--guard-magit-call-git 'magit-call-git)
    (advice-add 'magit-call-git :before #'skg--guard-magit-call-git))
  (unless (advice-member-p #'skg--guard-magit-start-git 'magit-start-git)
    (advice-add 'magit-start-git :before #'skg--guard-magit-start-git)))

(with-eval-after-load 'magit-process
  (skg--install-magit-worktree-guards))

(provide 'skg-worktree-guard)
;;; skg-worktree-guard.el ends here
