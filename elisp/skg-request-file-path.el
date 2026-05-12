;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Request the on-disk file path for a node,
;;; and open a magit view navigated to an appropriate location.

(require 'skg-id-search)
(require 'skg-readable-ids)
(require 'skg-state)
(require 'skg-length-prefix)

(defun skg-goto-in-magit ()
  "Open magit-status and go to the first section for the .skg file
of the node at point. If the file appears in multiple sections
\(unstaged, staged, untracked\), go to the first and warn the user."
  (interactive)
  (let (( info (skg--magit-node-info-at-point) ))
    (when info
      (skg--request-file-path-with-handler
       (car info) (cdr info) #'skg--magit-goto-handle-response))))

(defun skg-goto-in-magit-parent ()
  "Open magit-status on the parent's .skg file, and go to the first
line in the parent's magit section that contains this node's ID.
Warn the user if the ID appears multiple times in that section,
or if the parent has multiple sections.
If the ID does not appear at all, point is placed on the parent's
file section and the user is informed."
  (interactive)
  (let (( node-info (skg--magit-node-info-at-point) ))
    (when node-info
      (let (( node-id     (car node-info) )
            ( parent-info (skg--magit-parent-info-at-point) ))
        (when parent-info
          (skg--request-file-path-with-handler
           (car parent-info) (cdr parent-info)
           (lambda (tcp-proc payload)
             (skg--magit-goto-in-parent-handle-response
              node-id tcp-proc payload))))))))

(defun skg-goto-in-magit-and-close-this ()
  "Like `skg-goto-in-magit', but also kill the buffer it was called
from once the request has been issued."
  (interactive)
  (let ((buf (current-buffer)))
    (skg-goto-in-magit)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun skg-goto-in-magit-parent-and-close-this ()
  "Like `skg-goto-in-magit-parent', but also kill the buffer it was
called from once the request has been issued."
  (interactive)
  (let ((buf (current-buffer)))
    (skg-goto-in-magit-parent)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun skg--magit-node-info-at-point ()
  "Return a cons (id . source) for the node on the current line,
or nil (with a user message) if no valid node metadata is found."
  (save-excursion
    (beginning-of-line)
    (let (( sexp (skg-first-sexpr-on-line) ))
      (cond
       ((not sexp)
        (message "No metadata sexp found on this line.")
        nil)
       ((not (skg--metadata-sexp-contains-id-p sexp))
        (message "No id in this line's metadata.")
        nil)
       (t
        (let (( id     (skg--extract-id-from-metadata-sexp sexp) )
              ( source (skg--extract-source-from-metadata-sexp sexp) ))
          (if (and id source)
              (cons id source)
            (message "Could not extract id or source from metadata.")
            nil)))))))

(defun skg--magit-parent-info-at-point ()
  "Return a cons (id . source) for the node on the parent heading,
or nil (with a user message) if no valid parent metadata is found."
  (let (( parent-sexp nil ))
    (save-excursion
      (condition-case nil
          (progn (outline-up-heading 1)
                 (beginning-of-line)
                 (setq parent-sexp (skg-first-sexpr-on-line)))
        (error nil)))
    (cond
     ((not parent-sexp)
      (message "No parent heading found.")
      nil)
     ((not (skg--metadata-sexp-contains-id-p parent-sexp))
      (message "Parent heading has no node metadata.")
      nil)
     (t
      (let (( parent-id     (skg--extract-id-from-metadata-sexp parent-sexp) )
            ( parent-source (skg--extract-source-from-metadata-sexp parent-sexp) ))
        (if (and parent-id parent-source)
            (cons parent-id parent-source)
          (message "Could not extract id or source from parent.")
          nil))))))

(defun skg--request-file-path-with-handler (id source handler)
  "Send a get-file-path request for ID and SOURCE, using HANDLER for the response."
  (skg-register-response-handler
   'get-file-path handler t)
  (skg-lp-reset)
  (let* (( tcp-proc (skg-tcp-connect-to-rust) )
         ( request-sexp
           (concat (prin1-to-string
                    `((request . "get file path")
                      (id . ,id)
                      (source . ,source)))
                   "\n") ))
    (process-send-string tcp-proc request-sexp)))

(defun skg--magit-setup-from-response (payload)
  "Parse PAYLOAD, open magit-status on the response's file directory,
and collect the sections where the file appears in the magit buffer.
Returns a plist
  (:resolved-path PATH :rel-path REL :sections SECTIONS)
where SECTIONS is an ordered list of conses (KIND . SECTION),
KIND being \\='unstaged, \\='staged, or \\='untracked.
Returns nil (after a user message) on error or missing path."
  (let* (( response    (read payload) )
         ( raw-content (cadr (assoc 'content response)) )
         ( content     (and raw-content (format "%s" raw-content)) )
         ( path        (and content (string-trim content)) ))
    (cond
     ((not path) (message "No path received") nil)
     ((string-prefix-p "Error" path) (message "%s" path) nil)
     (t
      (unless (require 'magit nil t)
        (user-error "Magit is not installed or loadable"))
      (let* (( resolved-path    (expand-file-name path skg-config-dir) )
             ( default-directory ;; PITFALL: magit reads this implicitly to find the git repo via magit-toplevel. The .skg source dir is its own repo, separate from the outer project repo.
               (file-name-directory resolved-path) )
             ( repo-root (magit-toplevel default-directory) )
             ( rel-path  (file-relative-name resolved-path repo-root) ))
        (magit-status-setup-buffer default-directory)
        (skg-readable-ids-mode 1)
        (magit-section-show-level-2-all)
        (let (( sections
                (delq
                 nil
                 (list
                  (let (( s (skg--find-file-in-magit-section
                             '((unstaged) (status)) rel-path) ))
                    (and s (cons 'unstaged s)))
                  (let (( s (skg--find-file-in-magit-section
                             '((staged) (status)) rel-path) ))
                    (and s (cons 'staged s)))
                  (let (( s (skg--find-file-in-magit-section
                             '((untracked) (status)) rel-path) ))
                    (and s (cons 'untracked s))))) ))
          (list :resolved-path resolved-path
                :rel-path      rel-path
                :sections      sections)))))))

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

(defun skg--magit-kind-label (kind)
  "Human-readable label for a section KIND symbol."
  (cond ((eq kind 'unstaged)  "unstaged")
        ((eq kind 'staged)    "staged")
        ((eq kind 'untracked) "untracked")
        (t (format "%s" kind))))

(defun skg--magit-format-section-kinds (sections)
  "Format SECTIONS as a comma-separated list of kind labels."
  (mapconcat (lambda (s) (skg--magit-kind-label (car s)))
             sections ", "))

(defun skg--magit-goto-handle-response (_tcp-proc payload)
  "Handle the get-file-path response for `skg-goto-in-magit'.
Navigate to the first magit section for the file,
reporting the staging status and warning if the file appears
in multiple sections."
  (let (( ctx (skg--magit-setup-from-response payload) ))
    (when ctx
      (let* (( sections      (plist-get ctx :sections) )
             ( resolved-path (plist-get ctx :resolved-path) )
             ( n             (length sections) ))
        (cond
         ((= n 0)
          (message "magit: %s has not changed."
                   (file-name-nondirectory resolved-path)))
         ((= n 1)
          (magit-section-goto (cdr (car sections)))
          (message "Point is on %s changes."
                   (skg--magit-kind-label (car (car sections)))))
         (t
          (magit-section-goto (cdr (car sections)))
          (message "Warning: file appears in %d sections (%s); point is on the first (%s)."
                   n
                   (skg--magit-format-section-kinds sections)
                   (skg--magit-kind-label (car (car sections))))))))))

(defun skg--magit-goto-in-parent-handle-response (node-id _tcp-proc payload)
  "Handle the get-file-path response for `skg-goto-in-magit-parent'.
Navigate to the first line in the parent's first magit section
that contains NODE-ID. Warn if NODE-ID appears multiple times
there, or if the parent has multiple sections. If NODE-ID does
not appear at all, fall back to the parent's file section
and inform the user."
  (let (( ctx (skg--magit-setup-from-response payload) ))
    (when ctx
      (let* (( sections            (plist-get ctx :sections) )
             ( resolved-path       (plist-get ctx :resolved-path) )
             ( parent-section-count (length sections) ))
        (cond
         ((= parent-section-count 0)
          (message "Parent file %s has no unstaged, staged, or untracked changes."
                   (file-name-nondirectory resolved-path)))
         (t
          (let* (( first-entry   (car sections) )
                 ( first-kind    (car first-entry) )
                 ( first-section (cdr first-entry) )
                 ( match-count
                   (skg--magit-count-id-in-hunks first-section node-id) )
                 ( found
                   (skg--magit-goto-first-id-in-hunks first-section node-id) ))
            (cond
             ((not found)
              (magit-section-goto first-section)
              (message "%s not found in parent's %s section; point is on the parent file section.%s"
                       node-id
                       (skg--magit-kind-label first-kind)
                       (if (> parent-section-count 1)
                           (format " Parent also appears in: %s."
                                   (skg--magit-format-section-kinds (cdr sections)))
                         "")))
             (t
              (let (( warnings nil ))
                (when (> match-count 1)
                  (push (format "ID appears %d times in the %s section"
                                match-count
                                (skg--magit-kind-label first-kind))
                        warnings))
                (when (> parent-section-count 1)
                  (push (format "parent has %d sections (%s)"
                                parent-section-count
                                (skg--magit-format-section-kinds sections))
                        warnings))
                (if warnings
                    (message "Point on first appearance. Warning: %s."
                             (mapconcat #'identity
                                        (nreverse warnings) "; "))
                  (message "Point is on first appearance of %s in parent's %s section."
                           node-id
                           (skg--magit-kind-label first-kind)))))))))))))

(defun skg--magit-count-id-in-hunks (file-section id)
  "Count the lines in FILE-SECTION's hunks that contain ID.
Returns 0 if FILE-SECTION is nil or has no hunks."
  (if (not file-section)
      0
    (let (( count    0 )
          ( id-regex (regexp-quote id) ))
      (dolist (child (oref file-section children))
        (when (eq (oref child type) 'hunk)
          (save-excursion
            (goto-char (oref child start))
            (while (re-search-forward id-regex (oref child end) t)
              (setq count (1+ count))))))
      count)))

(defun skg--magit-goto-first-id-in-hunks (file-section id)
  "Expand FILE-SECTION and its hunks, then move point to the start of
the first line in any hunk that contains ID.
Returns non-nil on a match, nil otherwise."
  (when file-section
    (magit-section-show file-section)
    (dolist (child (oref file-section children))
      (magit-section-show child))
    (let (( id-regex (regexp-quote id) )
          ( found    nil ))
      (catch 'done
        (dolist (child (oref file-section children))
          (when (eq (oref child type) 'hunk)
            (goto-char (oref child start))
            (when (re-search-forward id-regex (oref child end) t)
              (beginning-of-line)
              (setq found t)
              (throw 'done t)))))
      found)))

(provide 'skg-request-file-path)
