;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: `git add' a skg node's .skg file, but only when that
;;; file is not yet known to git at all (untracked, not even in the
;;; index). Two user commands:
;;;
;;; - `skg-git-add-if-new' (bound to C-c t n in skg-content-view-mode):
;;;    act on the node at point.
;;; - `skg-git-add-if-new-recursive' (C-c t N): act on the node at
;;;    point and every org-descendant.
;;;
;;; "Not in the index" (rather than "not in HEAD") is the predicate,
;;; so that a previously-staged-but-modified file is NOT re-added by
;;; this command: the user's subsequent modifications would otherwise
;;; be silently staged. The command's job is to make the file exist
;;; in git history; that is a one-time thing, and once the file has
;;; been `git add'-ed (even if not committed), this command is done
;;; with it unless the user manually unstages.
;;;
;;; Path resolution happens entirely client-side via
;;; `skg--abs-path-for-id-and-source' (in skg-config.el), so the
;;; recursive version does not need a server round-trip per node.

(require 'cl-lib)
(require 'skg-config)
(require 'skg-id-search)      ;; skg--metadata-sexp-contains-id-p, etc.
(require 'skg-request-file-path) ;; skg--magit-node-info-at-point
(require 'skg-sexpr-search)   ;; skg-first-sexpr-on-line

(defun skg--git-inside-worktree-p (dir)
  "Return t if DIR is inside a git worktree."
  (let ((default-directory dir))
    (zerop (call-process "git" nil nil nil
                         "rev-parse" "--is-inside-work-tree"))))

(defun skg--git-untracked-p (abs-path)
  "Return t if ABS-PATH is outside the git index of its containing
worktree (i.e. neither staged nor in HEAD). Returns nil for any
file that git already knows about, regardless of whether the
worktree copy has been modified since."
  (let ((default-directory (file-name-directory abs-path))
        (file              (file-name-nondirectory abs-path)))
    (not (zerop (call-process "git" nil nil nil
                              "ls-files" "--error-unmatch" file)))))

(defun skg--git-add-file (abs-path)
  "Stage ABS-PATH in its git repo."
  (let ((default-directory (file-name-directory abs-path))
        (file              (file-name-nondirectory abs-path)))
    (call-process "git" nil nil nil "add" file)))

(defun skg-git-add-if-new ()
  "If the current node's .skg file is untracked in git, `git add' it.
Files already in the index (staged or committed) are left alone,
even if the worktree copy has been modified — we will not silently
stage those modifications. Also a no-op if the file is not on
disk, the source isn't declared in skgconfig.toml, or the source
directory isn't a git worktree."
  (interactive)
  (let ((info (skg--magit-node-info-at-point)))
    (when info
      (skg--add-if-new-by-id-and-source (car info) (cdr info)))))

(defun skg--add-if-new-by-id-and-source (id source)
  "`git add' the .skg file for ID in SOURCE if it is untracked.
Returns one of: 'added, 'already-in-index, 'missing, 'no-source, 'not-a-repo."
  (let ((path (skg--abs-path-for-id-and-source id source)))
    (cond
     ((null path)
      (message "skg-git-add-if-new: source %S not in skgconfig.toml" source)
      'no-source)
     ((not (file-exists-p path))
      (message "skg-git-add-if-new: %s not on disk"
               (file-name-nondirectory path))
      'missing)
     ((not (skg--git-inside-worktree-p (file-name-directory path)))
      (message "skg-git-add-if-new: %s is not inside a git worktree"
               (file-name-nondirectory path))
      'not-a-repo)
     ((not (skg--git-untracked-p path))
      (message "skg-git-add-if-new: %s already in git"
               (file-name-nondirectory path))
      'already-in-index)
     (t
      (skg--git-add-file path)
      (message "skg-git-add-if-new: added %s"
               (file-name-nondirectory path))
      'added))))

(defun skg-git-add-if-new-recursive ()
  "Like `skg-git-add-if-new' but also applies to every org-descendant
of the current heading. Headings without id+source metadata are
skipped silently."
  (interactive)
  (let ((pairs (skg--subtree-id-and-source-pairs))
        (added 0) (in-index 0) (missing 0) (no-source 0) (not-repo 0))
    (dolist (pair pairs)
      (pcase (skg--add-if-new-by-id-and-source (car pair) (cdr pair))
        ('added            (cl-incf added))
        ('already-in-index (cl-incf in-index))
        ('missing          (cl-incf missing))
        ('no-source        (cl-incf no-source))
        ('not-a-repo       (cl-incf not-repo))))
    (message
     "skg-git-add-if-new-recursive: %d added, %d already in git, %d missing on disk, %d unknown source, %d not a repo"
     added in-index missing no-source not-repo)))

(defun skg--subtree-id-and-source-pairs ()
  "Return a list of (id . source) for the current heading and every
org-descendant whose headline metadata carries both an id and a
source. Order is the natural outline order."
  (save-excursion
    (let ((pairs '()))
      (org-back-to-heading t)
      (org-map-entries
       (lambda ()
         (save-excursion
           (beginning-of-line)
           (let ((sexp (skg-first-sexpr-on-line)))
             (when sexp
               (let ((id     (skg--extract-id-from-metadata-sexp sexp))
                     (source (skg--extract-source-from-metadata-sexp sexp)))
                 (when (and id source)
                   (push (cons id source) pairs)))))))
       nil 'tree)
      (nreverse pairs))))

(provide 'skg-git-add)
