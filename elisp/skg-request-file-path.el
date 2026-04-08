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
and opens a magit diff of just that file."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((sexp (skg-first-sexpr-on-line)))
      (if (not sexp)
          (message "No metadata sexp found on this line.")
        (if (not (skg--metadata-sexp-contains-id-p sexp))
            (message "Not a truenode line (no id in metadata).")
          (let ((id     (skg--extract-id-from-metadata-sexp sexp))
                (source (skg--extract-source-from-metadata-sexp sexp)))
            (if (not (and id source))
                (message "Could not extract id or source from metadata.")
              (skg-register-response-handler
               'get-file-path
               #'skg--file-diff-result
               t)
              (skg-lp-reset)
              (let* ((tcp-proc (skg-tcp-connect-to-rust))
                     (request-sexp
                      (concat (prin1-to-string
                               `((request . "get file path")
                                 (id . ,id)
                                 (source . ,source)))
                              "\n")))
                (process-send-string tcp-proc request-sexp)) )) )) )) )

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

(provide 'skg-request-file-path)
