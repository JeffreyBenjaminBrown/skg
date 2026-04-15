;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Request the on-disk file path for the node at point,
;;; and open a magit diff of that file.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-view-magit
;;;
;;; NOTE: The path-extraction logic (getting id and source from the
;;; current line's metadata) could be reused for other commands,
;;; e.g. visiting the .skg file directly via `find-file'.

(require 'skg-id-search)
(require 'skg-state)
(require 'skg-length-prefix)

(defun skg-view-magit ()
  "Open a magit diff for the .skg file of the node at point.
Reads the (skg ...) metadata from the current line, extracts
the id and source, asks the server to resolve the file path,
and opens magit-status navigated to that file --
unless it was called from a RemovedHere phantoms,
in which case it navigates to the *parent's* diff,
and positions point on the deletion line for this node's ID."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((sexp (skg-first-sexpr-on-line)))
      (if (not sexp)
          (message "No metadata sexp found on this line.")
        (if (not (skg--metadata-sexp-contains-id-p sexp))
            (message "Not a truenode line (no id in metadata).")
          (let ((diff (skg--extract-diff-from-metadata-sexp sexp)))
            (if (equal diff "removed-here")
                (skg--view-magit-for-removed-here-phantom sexp)
              (skg--view-magit-for-normal-node sexp))))))))

(defun skg--view-magit-for-normal-node (sexp)
  "Request the file path for SEXP's node and open its magit diff."
  (let ((id     (skg--extract-id-from-metadata-sexp sexp))
        (source (skg--extract-source-from-metadata-sexp sexp)))
    (if (not (and id source))
        (message "Could not extract id or source from metadata.")
      (skg--request-file-path-with-handler
       id source #'skg--file-diff-result))))

(defun skg--view-magit-for-removed-here-phantom (phantom-sexp)
  "For a RemovedHere phantom, open magit on the parent's file
and navigate to the deletion line for the phantom's ID."
  (let ((phantom-id (skg--extract-id-from-metadata-sexp phantom-sexp))
        (parent-sexp nil))
    (save-excursion
      (condition-case nil
          (progn (outline-up-heading 1)
                 (beginning-of-line)
                 (setq parent-sexp (skg-first-sexpr-on-line)))
        (error nil)))
    (if (not parent-sexp)
        (message "No parent heading found for phantom.")
      (if (not (skg--metadata-sexp-contains-id-p parent-sexp))
          (message "Parent heading has no node metadata.")
        (let ((parent-id     (skg--extract-id-from-metadata-sexp parent-sexp))
              (parent-source (skg--extract-source-from-metadata-sexp parent-sexp)))
          (if (not (and parent-id parent-source))
              (message "Could not extract id or source from parent.")
            (skg--request-file-path-with-handler
             parent-id parent-source
             (lambda (_tcp-proc payload)
               (skg--handle-removedhere-filepath-response
                phantom-id _tcp-proc payload)))))))))

(defun skg--request-file-path-with-handler (id source handler)
  "Send a get-file-path request for ID and SOURCE, using HANDLER for the response."
  (skg-register-response-handler
   'get-file-path handler t)
  (skg-lp-reset)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp
          (concat (prin1-to-string
                   `((request . "get file path")
                     (id . ,id)
                     (source . ,source)))
                  "\n")))
    (process-send-string tcp-proc request-sexp)))

(defun skg--find-file-in-magit-section
    (parent-ident ;; the section identity path that magit-get-section uses to find the parent section
     filename)
  "Find a file section with FILENAME under the magit section at PARENT-IDENT.
Returns the section object, or nil."
  (let (( parent (magit-get-section parent-ident) ))
    (when parent
      (cl-find-if
       (lambda (child)
         (and (eq (oref child type) 'file)
              (equal (oref child value) filename)) )
       (oref parent children)) )))

(defun skg--file-diff-result (_tcp-proc payload)
  "Handle the server response for a get-file-path request,
by opening magit-status and navigates to the file.
PAYLOAD is the tagged LP response from the server."
  (let* (( response (read payload) )
         ( raw-content (cadr (assoc 'content response)) )
         ( content (and raw-content (format "%s" raw-content)) )
         ( path (and content (string-trim content)) ))
    (cond
     ((not path)
      (message "No path received"))
     ((string-prefix-p "Error" path)
      (message "%s" path))
     (t
      (let* (( resolved-path (expand-file-name path skg-config-dir) )
             ( default-directory ;; PITFALL: magit reads this implicitly to find the git repo via magit-toplevel. The .skg source dir is its own repo, separate from the outer project repo.
               (file-name-directory resolved-path) )
             ( repo-root (magit-toplevel default-directory) )
             ( rel-path (file-relative-name resolved-path repo-root) ))
        (magit-status-setup-buffer default-directory)
        (magit-section-show-level-2-all)
        (let (( unstaged-section
               (skg--find-file-in-magit-section
                '((unstaged) (status)) rel-path) )
              ( staged-section
               (skg--find-file-in-magit-section
                '((staged) (status)) rel-path) )
              ( untracked-section
               (skg--find-file-in-magit-section
                '((untracked) (status)) rel-path) ))
          (cond
           (unstaged-section
            (magit-section-goto unstaged-section)
            (if staged-section
                (message "Point is on unstaged changes. Some changes are staged, too.")
              (message "Point is on unstaged changes. No changes are staged.") ))
           (staged-section
            (magit-section-goto staged-section)
            (message "Point is on staged changes. No changes are unstaged.") )
           (untracked-section
            (magit-section-goto untracked-section)
            (message "Point is on untracked (new) file.") )
           (t
            (message "magit: %s has not changed."
                     (file-name-nondirectory resolved-path)) )) )) )) ))

(defun skg--handle-removedhere-filepath-response (phantom-id _tcp-proc payload)
  "Handle get-file-path response for a RemovedHere phantom.
Opens magit on the parent's file and navigates to the line
showing deletion of PHANTOM-ID."
  (let* (( response (read payload) )
         ( raw-content (cadr (assoc 'content response)) )
         ( content (and raw-content (format "%s" raw-content)) )
         ( path (and content (string-trim content)) ))
    (cond
     ((not path)
      (message "No path received"))
     ((string-prefix-p "Error" path)
      (message "%s" path))
     (t
      (let* (( resolved-path (expand-file-name path skg-config-dir) )
             ( default-directory
               (file-name-directory resolved-path) )
             ( repo-root (magit-toplevel default-directory) )
             ( rel-path (file-relative-name resolved-path repo-root) )
             ( deletion-regex
               (concat "^-" (regexp-quote (concat "- " phantom-id)) "$") ))
        (magit-status-setup-buffer default-directory)
        (magit-section-show-level-2-all)
        (let (( unstaged-section
               (skg--find-file-in-magit-section
                '((unstaged) (status)) rel-path) )
              ( staged-section
               (skg--find-file-in-magit-section
                '((staged) (status)) rel-path) ))
          (or (skg--goto-deletion-in-magit-section
               unstaged-section deletion-regex)
              (skg--goto-deletion-in-magit-section
               staged-section deletion-regex)
              (cond
               (unstaged-section
                (magit-section-goto unstaged-section)
                (message "Deletion of %s not found in diff; point is on parent file."
                         phantom-id))
               (staged-section
                (magit-section-goto staged-section)
                (message "Deletion of %s not found in diff; point is on parent file."
                         phantom-id))
               (t
                (message "Parent file %s has no unstaged or staged changes."
                         (file-name-nondirectory resolved-path)))))))))))

(defun skg--goto-deletion-in-magit-section (file-section deletion-regex)
  "In FILE-SECTION, expand hunks and search for DELETION-REGEX.
Returns non-nil and moves point if found; returns nil otherwise."
  (when file-section
    (magit-section-show file-section)
    (dolist (child (oref file-section children))
      (magit-section-show child))
    (goto-char (oref file-section start))
    (when (re-search-forward deletion-regex (oref file-section end) t)
      (beginning-of-line)
      t)))

(provide 'skg-request-file-path)
