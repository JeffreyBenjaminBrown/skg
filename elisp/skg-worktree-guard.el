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

(defun skg--dirty-view-buffers ()
  "Return every live Skg view with unsaved changes."
  (cl-remove-if-not
   (lambda (buffer)
     (and (skg-buffer-p buffer)
          (buffer-modified-p buffer)))
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
    (skg--refuse-worktree-write-if-views-dirty "Raw .skg save")))

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
