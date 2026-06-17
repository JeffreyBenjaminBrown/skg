;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: `git add' new .skg files in a subtree, but only when
;;; each file is not yet known to git at all (untracked, not even in
;;; the index). User commands:
;;;
;;; - `skg-git-add-if-new-recursive' (C-c t A): act on the node at
;;;    point and every org-descendant.
;;; - `skg-git-add-if-new-recursive-preview' (C-c t a):
;;;    show a git-add command for all new (untracked) files,
;;;    in 'skg-readable-ids-mode'.
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
(require 'skg-readable-ids)
(require 'skg-sexpr-search)   ;; skg-first-sexpr-on-line

(defun skg-git-add-if-new-recursive ()
  "Run new-file git-add command that
`skg-git-add-if-new-recursive-preview' would display.
(The idea is that once you trust
'skg-git-add-if-new-recursive-preview',
you might not want to review the command manually.)"
  (interactive)
  (let* ((plan (skg--git-add-new-files-recursive-plan))
         (paths (plist-get plan :paths))
         (form (plist-get plan :form)))
    (eval form t)
    (message "skg-git-add-if-new-recursive: ran git-add command for %d new file(s)"
             (length paths))))

(defun skg-git-add-if-new-recursive-preview ()
  "Show an executable buffer that stages new files in the current subtree.
The current heading and its org-descendants are scanned for ActiveNode
metadata containing `(unstaged newX)'. The generated form rechecks
the git index before staging each file, so evaluating it will not
stage later modifications to files that are already known to git."
  (interactive)
  (let* ((plan (skg--git-add-new-files-recursive-plan))
         (paths (plist-get plan :paths))
         (buffer (get-buffer-create "*skg git add new files*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emacs-lisp-mode)
        ;; The preview is meant to be evaluated later. If user/custom
        ;; printer limits are active, `pp-to-string' can emit `...'
        ;; inside the quoted filename list, making the buffer invalid
        ;; as executable code.
        (let ((print-length nil)
              (print-level nil))
          (insert (pp-to-string (plist-get plan :form))))
        (goto-char (point-min))
        (skg-readable-ids-mode 1)))
    (pop-to-buffer buffer)
    (message "skg-git-add-if-new-recursive-preview: %d new file(s)"
             (length paths))))

(defun skg--git-add-new-files-recursive-plan ()
  "Return the executable git-add plan for new files in this subtree.
The result is a plist with :paths and :form keys."
  (let* ((pairs (skg--subtree-unstaged-new-file-id-and-source-pairs))
         (paths (skg--abs-paths-for-id-source-pairs pairs)))
    (list :paths paths
          :form (skg--git-add-new-files-form paths))))

(defun skg--git-add-new-files-form (abs-paths)
  "Return executable Emacs Lisp that stages ABS-PATHS only if unindexed."
  (let ((forms
         (mapcar #'skg--git-add-form-for-source-dir
                 (skg--group-abs-paths-by-dir abs-paths))))
    (if forms
        (cons 'progn forms)
      '(message "No unstaged new skg files found in this subtree."))))

(defun skg--git-add-form-for-source-dir (dir-and-files)
  "Return one executable git-add form for DIR-AND-FILES.
DIR-AND-FILES is (DIR . FILES), where FILES are basenames."
  (let ((dir (car dir-and-files))
        (files (cdr dir-and-files)))
    `(let ((default-directory ,dir))
       (dolist (file ',files)
         (unless
             (zerop
              (call-process
               "git" nil nil nil
               "ls-files" "--error-unmatch" file))
           (call-process
            "git" nil "*skg-git-add-output*" t
            "add" "--" file))))))

(defun skg--group-abs-paths-by-dir (abs-paths)
  "Group ABS-PATHS by directory, preserving first-seen directory order."
  (let ((groups nil)
        (dirs nil))
    (dolist (path abs-paths)
      (let* ((dir (file-name-as-directory
                   (file-name-directory path)))
             (file (file-name-nondirectory path))
             (existing (assoc dir groups)))
        (if existing
            (setcdr existing (append (cdr existing) (list file)))
          (push dir dirs)
          (push (cons dir (list file)) groups))))
    (mapcar (lambda (dir) (assoc dir groups))
            (nreverse dirs))))

(defun skg--abs-paths-for-id-source-pairs (pairs)
  "Return unique absolute .skg paths for PAIRS of (ID . SOURCE)."
  (let ((paths nil))
    (dolist (pair pairs)
      (let ((path (skg--abs-path-for-id-and-source
                   (car pair) (cdr pair))))
        (when path
          (push path paths))))
    (delete-dups (nreverse paths))))

(defun skg--subtree-unstaged-new-file-id-and-source-pairs ()
  "Return (id . source) pairs in this subtree whose metadata has unstaged newX.
This collects file-existence changes, not membership-only `newM'
changes."
  (skg--subtree-id-and-source-pairs-if
   #'skg--metadata-has-unstaged-new-file-p))

(defun skg--subtree-id-and-source-pairs ()
  "Return a list of (id . source) for the current heading and every
org-descendant whose headline metadata carries both an id and a
source. Order is the natural outline order."
  (skg--subtree-id-and-source-pairs-if
   (lambda (_sexp) t)))

(defun skg--subtree-id-and-source-pairs-if (predicate)
  "Return subtree (id . source) pairs whose metadata satisfies PREDICATE.
Order is the natural outline order."
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
                 (when (and id source
                            (funcall predicate sexp))
                   (push (cons id source) pairs)))))))
       nil 'tree)
      (nreverse pairs))))

(defun skg--metadata-has-unstaged-new-file-p (sexp)
  "Return t if SEXP is an ActiveNode marked as a new worktree file."
  (memq 'newX (skg-sexp-cdr-at-path sexp '(skg node unstaged))))

(provide 'skg-git-add)
