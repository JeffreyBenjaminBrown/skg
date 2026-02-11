;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Request the on-disk file path for the node at point,
;;; and open a magit diff of that file.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-visit-file-diff
;;;
;;; NOTE: The path-extraction logic (getting id and source from the
;;; current line's metadata) could be reused for other commands,
;;; e.g. visiting the .skg file directly via `find-file'.

(require 'skg-id-search)
(require 'skg-state)

(defun skg-visit-file-diff ()
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
          (let ((id     (skg--extract-id-from-sexp sexp))
                (source (skg--extract-source-from-sexp sexp)))
            (if (not (and id source))
                (message "Could not extract id or source from metadata.")
              (setq skg-doc--response-handler
                    #'skg--file-diff-result)
              (let* ((tcp-proc (skg-tcp-connect-to-rust))
                     (request-sexp
                      (concat (prin1-to-string
                               `((request . "get file path")
                                 (id . ,id)
                                 (source . ,source)))
                              "\n")))
                (process-send-string tcp-proc request-sexp)) )) )) )) )

(defun skg--file-diff-result (_tcp-proc string)
  "Handle the server response for a get-file-path request.
STRING is the response: either a relative path or an error message."
  (let ((response (string-trim string)))
    (if (string-prefix-p "File not found" response)
        (message "%s" response)
      (if (string-prefix-p "Error" response)
          (message "%s" response)
        (let ((resolved-path
               (expand-file-name response skg-config-dir)))
          (let ((default-directory ;; DANGER: magit reads this implicitly to find the git repo via magit-toplevel. The .skg source dir is its own repo, separate from the outer project repo.
                 (file-name-directory resolved-path)))
            (magit-diff-range "HEAD" nil (list resolved-path)) )) )) ))

(provide 'skg-request-file-path)
