;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: `skg-describe-at-point' (C-c v ?) -- say in plain sentences
;;; what the headline at point is, when it is a folder, and what each of
;;; its heralds means, with this headline's own numbers and letters read
;;; out. The sentences come from the server's "describe heralds"
;;; endpoint ('docs/api-and-formats.org', "Describe heralds";
;;; 'server/herald_descriptions.rs'), which Neovim and skg-for-ai ask
;;; too, so that they are written once.

(require 'org)
(require 'skg-length-prefix)
(require 'skg-metadata)
(require 'skg-change-facts)

(defconst skg-describe-buffer-name "*skg describe*"
  "The help buffer `skg-describe-at-point' shows long descriptions in.")

(defun skg-describe-at-point ()
  "Describe the headline at point, or the one whose body point is in:
what it is, when it is a folder, then one sentence per herald it
shows, the most informative first. Shows them in the echo area when
they fit in two lines, else in the buffer `skg-describe-buffer-name'.
It reads the headline's metadata as the buffer holds it, so it works
with heralds shown or hidden; it changes nothing and saves nothing."
  (interactive)
  (let ((request (skg--describe-request-at-point)))
    (skg-register-response-handler
     'describe-heralds
     (lambda (_tcp-proc payload)
       (let ((lines-or-error (skg--describe-lines-of-response payload)))
         (run-at-time 0 nil #'skg--describe-show lines-or-error)))
     t)
    (skg-lp-reset)
    (process-send-string (skg-tcp-connect-to-rust)
                         (concat (prin1-to-string request) "\n"))))

(defun skg--describe-request-at-point ()
  "The \"describe heralds\" request for the headline at point: its
metadata as the buffer holds it, with the atom `pendingDetachment' when
it is a pending detachment (`skg--pending-detachment-at-point-p'), and,
for a folder of the diff view whose members' relationships changed,
whether every such change is processed."
  (save-excursion
    (condition-case nil
        (org-back-to-heading t)
      (error (user-error "Not on or under a headline")))
    (let ((metadata (skg--metadata-on-line)))
      (unless metadata
        (user-error "This headline carries no skg metadata"))
      (let ((text (buffer-substring-no-properties (nth 0 metadata)
                                                  (nth 1 metadata)))
            (folder-change (skg--folder-change-processed-at-point)))
        `((request . "describe heralds")
          (metadata . ,(if (skg--pending-detachment-at-point-p)
                           (concat (substring text 0 -1) " pendingDetachment)")
                         text))
          ,@(when folder-change
              `((folder-change . ,(symbol-name folder-change)))))))))

(defun skg--describe-lines-of-response (payload)
  "The sentences of PAYLOAD, a describe-heralds response, as a list of
strings, or (error MESSAGE)."
  (let* ((response (read payload))
         (err (cadr (assoc 'error response))))
    (if err
        (list 'error (format "%s" err))
      (cdr (assoc 'lines response)))))

(defun skg--describe-show (lines-or-error)
  "Show LINES-OR-ERROR, from `skg--describe-lines-of-response': in the
echo area when it fits in two lines of the frame, else in the buffer
`skg-describe-buffer-name', a sentence to a paragraph."
  (cond
   ((eq (car-safe lines-or-error) 'error)
    (message "skg describe: %s" (cadr lines-or-error)))
   ((skg--describe-fits-echo-area-p lines-or-error)
    (message "%s" (mapconcat #'identity lines-or-error "\n")))
   (t
    (with-help-window skg-describe-buffer-name
      (with-current-buffer standard-output
        (let ((inhibit-read-only t))
          (dolist (line lines-or-error)
            (unless (bobp) (insert "\n"))
            (let ((start (point)))
              (insert line "\n")
              (fill-region start (point))))))))))

(defun skg--describe-fits-echo-area-p (lines)
  "Whether LINES, one per line, take at most two lines of the frame."
  (let ((width (max 1 (1- (frame-width)))))
    (<= (apply #'+ (mapcar (lambda (line)
                             (max 1 (ceiling (string-width line)
                                             (float width))))
                           lines))
        2)))

(provide 'skg-request-describe-heralds)
