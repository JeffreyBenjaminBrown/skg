;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: skg-export-some-to-org -- export Skg data to plain .org
;;; files under org-exports/. All the logic is in Rust; this command
;;; picks a source-set (client-side, with the circular selector) and
;;; sends the "export to org" request, then shows the server's report.

(require 'skg-config)         ; skg--prompt-for-source-set
(require 'skg-length-prefix)
(require 'skg-request-save)   ; skg-big-nonfatal-message,
                              ; skg--message-list-nonempty-p,
                              ; skg-errors-and-warnings-to-org-string
(require 'skg-state)

(defun skg-export-some-to-org (source-set output-dir)
  "Export Skg data to .org files under OUTPUT-DIR.
Prompts for a SOURCE-SET (S-left/S-right cycle, tab completion);
only nodes from that set are included, so choose a public-facing
subset to avoid exporting private notes.  Then prompts for
OUTPUT-DIR.

OUTPUT-DIR is interpreted *by the server*, relative to the project
root the server runs from -- which may be inside a Docker
container, so this prompt has no tab completion (the host
filesystem would not match).  An absolute path is used as-is.  The
default is `org-exports'.

The Rust server finds every export root -- a node with a child
whose title links to the export-instruction node and whose body
gives a `target_filepath' -- and writes each to
OUTPUT-DIR/<target_filepath>.org as a recursive content view,
stripped of skg metadata, with links rewritten to relative org
links.  Broken links point to a \"some links might be broken\"
note.  Existing files are overwritten in place; others are left
untouched."
  (interactive
   (list (skg--prompt-for-source-set)
         (read-string
          "Export to dir (relative to the server's project root) [org-exports]: "
          nil nil "org-exports")))
  (when (string= source-set "all")
    (message
     "Exporting source-set `all'; consider a public-facing source-set so private notes are not exported."))
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'export-to-org
     #'skg--export-to-org-handler
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "export to org")
         (source-set . ,source-set)
         (output-dir . ,output-dir)))
      "\n"))))

(defun skg--export-to-org-handler (_tcp-proc payload)
  "Display an export-to-org response PAYLOAD."
  (condition-case err
      (let* ((response (read payload))
             (content (cadr (assoc 'content response)))
             (errors-list (cadr (assoc 'errors response)))
             (warnings-list (cadr (assoc 'warnings response)))
             (has-errors (skg--message-list-nonempty-p errors-list))
             (has-warnings (skg--message-list-nonempty-p warnings-list)))
        (skg-big-nonfatal-message
         "*skg export to org*"
         (cond
          (has-errors "Export to org completed with errors")
          (has-warnings "Export to org completed with warnings")
          (t "Export to org complete"))
         (or content "Export produced no output."))
        (when (or has-errors has-warnings)
          (skg-big-nonfatal-message
           "*skg export to org messages*"
           "Export to org messages"
           (skg-errors-and-warnings-to-org-string
            errors-list warnings-list))))
    (error
     (message "skg: export-to-org handler error: %S" err))))

(provide 'skg-request-export-org)
