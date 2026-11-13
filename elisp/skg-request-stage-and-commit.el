;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Stage every change and git-commit it, in every owned
;;; skgrepo that has changes, with one message
;;; ('docs/diff-view.org', "Stage and commit").
;;; The message is written in a buffer of its own, as in magit:
;;; C-c C-c sends it and C-c C-k cancels.
;;; When the processed summary is incomplete, or some owned gitrepo
;;; has entries already staged, the server asks for confirmation
;;; instead and changes nothing; only the typed word "yes" resends the
;;; request confirmed. Every answer is followed by a rerender
;;; stream, which after a commit rerenders the open buffers of the
;;; diff view.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-buffer)
(require 'skg-processed) ; for skg--processed-summary-message
(require 'skg-request-save) ; for skg-big-nonfatal-message
(require 'skg-request-rerender-all-views)

(defconst skg--commit-message-buffer-name "*skg commit message*"
  "The buffer in which `skg-stage-and-commit' takes the commit message.")

(defvar skg-commit-message-mode-map (make-sparse-keymap)
  "Keymap for `skg-commit-message-mode'.")

(let ((map skg-commit-message-mode-map))
  (setcdr map nil)
  (define-key map (kbd "C-c C-c") #'skg-commit-message-send)
  (define-key map (kbd "C-c C-k") #'skg-commit-message-cancel))

(define-derived-mode skg-commit-message-mode text-mode "skg-commit-message"
  "The buffer in which `skg-stage-and-commit' takes the commit message.
\\<skg-commit-message-mode-map>\\[skg-commit-message-send] stages and commits, with the buffer's text as
the message; \\[skg-commit-message-cancel] cancels."
  (setq header-line-format
        (substitute-command-keys
         "\\<skg-commit-message-mode-map>Commit message: \\[skg-commit-message-send] stages and commits, \\[skg-commit-message-cancel] cancels")))

(defun skg-stage-and-commit ()
  "Stage every change and git-commit it, in every owned skgrepo
that has changes, with one message, written in the buffer
`skg--commit-message-buffer-name' (see `skg-commit-message-mode').
A message left there by an earlier attempt is kept. If not every
changed file is processed, or some owned gitrepo has entries
already staged, show the processed summary and those entries, and
proceed only if the user types \"yes\"."
  (interactive)
  (skg--stage-and-commit-require-saved-views)
  (let ((buffer (get-buffer-create skg--commit-message-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'skg-commit-message-mode)
        (skg-commit-message-mode)))
    (pop-to-buffer buffer)))

(defun skg-commit-message-send ()
  "Stage and commit with this buffer's text as the commit message, less
its trailing whitespace; then hide the buffer, restoring its window.
The buffer is killed once the message is moot
\(`skg--stage-and-commit-report-result'); until then it keeps the text,
for the next `skg-stage-and-commit'. A blank message is refused."
  (interactive)
  (skg--commit-message-require-mode)
  (let ((commit-message
         (string-trim-right
          (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p commit-message)
      (user-error "A commit message is required"))
    (skg--stage-and-commit-require-saved-views)
    (skg--stage-and-commit-send commit-message nil)
    (skg--commit-message-buffer-hide)))

(defun skg-commit-message-cancel ()
  "Kill this buffer, restoring its window, and commit nothing."
  (interactive)
  (skg--commit-message-require-mode)
  (skg--commit-message-buffer-kill)
  (message "Commit canceled; nothing was staged or committed."))

(defun skg--stage-and-commit-require-saved-views ()
  "Refuse while some skg view has unsaved edits: the rerender after a
commit would discard them."
  (when (skg--unsaved-view-buffers)
    (user-error "Save or revert every skg view before staging and committing")))

(defun skg--commit-message-require-mode ()
  "Refuse outside a buffer in `skg-commit-message-mode'."
  (unless (derived-mode-p 'skg-commit-message-mode)
    (user-error "Not in the skg commit-message buffer")))

(defun skg--commit-message-buffer-hide ()
  "Hide the current buffer, keeping it: a window showing it goes back
to what it showed before, as `quit-window' does."
  (let ((window (get-buffer-window (current-buffer))))
    (when window (quit-window nil window))))

(defun skg--commit-message-buffer-kill ()
  "Kill the current buffer; a window showing it goes back to what it
showed before, as `quit-window' does."
  (let ((window (get-buffer-window (current-buffer))))
    (if window
        (quit-window t window)
      (kill-buffer (current-buffer)))))

(defun skg--stage-and-commit-send (commit-message confirmed)
  "Send the request with COMMIT-MESSAGE, and '(confirmed . t)' when
CONFIRMED. The message goes one string per line, as
'(message-lines \"LINE\" ...)', since the server reads one request
per line."
  (skg--send-streaming-request
   "stage and commit"
   'stage-and-commit
   (append `((request . "stage and commit")
             (message-lines ,@(split-string commit-message "\n")))
           (when confirmed '((confirmed . t))))
   (lambda (_tcp payload)
     (skg--stage-and-commit-handler commit-message payload))
   nil t))

(defun skg--stage-and-commit-handler (commit-message payload)
  "Handle the stage-and-commit response PAYLOAD. A request for
confirmation is asked about once the empty rerender stream that
follows is done, and \"yes\" resends COMMIT-MESSAGE confirmed."
  (condition-case err
      (let ((response (read payload)))
        (if (assq 'needs-confirmation response)
            ;; PITFALL: asking here, inside the process filter, would let
            ;; a quit at the prompt escape the filter with the rest of the
            ;; stream unread, leaving the stream in progress.
            (setq skg--rerender-after-empty-stream
                  (lambda ()
                    (skg--stage-and-commit-confirm-and-resend
                     commit-message response)))
          (skg--stage-and-commit-report-result response)))
    (error (message "skg: stage-and-commit handler error: %S" err))))

(defun skg--stage-and-commit-report-result (response)
  "Show the line for RESPONSE, a result. Once something was committed,
or nothing needed committing and nothing failed, the commit message is
moot and its buffer is killed; otherwise the buffer keeps it, and the
line says so."
  (let ((message-moot (or (cadr (assq 'commits response))
                          (null (skg--response-errors-line response)))))
    (when message-moot
      (let ((buffer (get-buffer skg--commit-message-buffer-name)))
        (when buffer (kill-buffer buffer))))
    (message "%s" (string-trim-right
                   (concat (skg--stage-and-commit-result-message response)
                           " "
                           (if message-moot ""
                             (skg--commit-message-kept-sentence "Your")))))))

(defun skg--commit-message-kept-sentence (first-word)
  "Where the commit message is kept, as a sentence opening with
FIRST-WORD, or the empty string when its buffer is gone."
  (if (get-buffer skg--commit-message-buffer-name)
      (format "%s message is kept in %s." first-word
              skg--commit-message-buffer-name)
    ""))

(defconst skg--stage-and-commit-already-staged-buffer-name
  "*skg already staged*"
  "The buffer listing every path already staged, by gitrepo, when the
server asks to confirm a commit that would include them.")

(defun skg--stage-and-commit-confirm-and-resend (commit-message response)
  "Ask to confirm RESPONSE, a request for confirmation, and on \"yes\"
resend COMMIT-MESSAGE confirmed. Otherwise, a quit at the prompt
included, say that nothing was committed, and where the message is
kept."
  (if (condition-case nil
          (skg--stage-and-commit-confirmed-p response)
        (quit nil))
      (skg--stage-and-commit-send commit-message t)
    (let ((kept (skg--commit-message-kept-sentence "your")))
      (message "%s" (if (string-empty-p kept) "Not committed."
                      (concat "Not committed; " kept))))))

(defun skg--stage-and-commit-confirmed-p (response)
  "Show the processed summary of RESPONSE, a request for confirmation,
and how many entries it lists as already staged in each gitrepo, the
entries themselves in the buffer
`skg--stage-and-commit-already-staged-buffer-name'; non-nil iff the
user types exactly \"yes\"."
  (let ((already-staged (cadr (assq 'already-staged response))))
    (when already-staged
      (skg-big-nonfatal-message
       skg--stage-and-commit-already-staged-buffer-name ""
       (skg--stage-and-commit-already-staged-listing already-staged)))
    (string= "yes"
             (read-string
              (format "%s%s. Type yes to commit anyway: "
                      (string-remove-suffix
                       "." (skg--processed-summary-message
                            (assq 'processed-summary response)))
                      (skg--stage-and-commit-already-staged-message
                       already-staged))))))

(defun skg--stage-and-commit-already-staged-message (already-staged)
  "The prompt's clause for ALREADY-STAGED, '((GITREPO PATH...) ...)':
how many paths each gitrepo has staged, and where they are listed; or
the empty string when there is none."
  (if (null already-staged) ""
    (format ". Already staged, so committed too: %s (listed in %s)"
            (mapconcat
             (lambda (gitrepo-and-paths)
               (let ((count (length (cdr gitrepo-and-paths))))
                 (format "%s: %d %s" (car gitrepo-and-paths) count
                         (if (= count 1) "path" "paths"))))
             already-staged ", ")
            skg--stage-and-commit-already-staged-buffer-name)))

(defun skg--stage-and-commit-already-staged-listing (already-staged)
  "ALREADY-STAGED, '((GITREPO PATH...) ...)', as org text: a headline per
gitrepo, then its paths, one per line."
  (mapconcat
   (lambda (gitrepo-and-paths)
     (concat (format "* %s\n" (car gitrepo-and-paths))
             (mapconcat (lambda (path) (format "%s\n" path))
                        (cdr gitrepo-and-paths) "")))
   already-staged ""))

(defun skg--stage-and-commit-result-message (response)
  "The line shown for RESPONSE, '((commits ((SKGREPO COMMIT-ID) ...))
\(errors (...)) (warnings (...)))'."
  (let ((commits (cadr (assq 'commits response)))
        (errors-line (skg--response-errors-line response)))
    (concat
     (if commits
         (format "Committed in %s"
                 (mapconcat
                  (lambda (commit)
                    (let ((commit-id (format "%s" (cadr commit))))
                      (format "%s (%s)" (car commit)
                              (substring commit-id 0 (min 7 (length commit-id))))))
                  commits ", "))
       "Nothing was committed")
     (if errors-line
         (format ". Errors: %s" errors-line)
       "."))))

(provide 'skg-request-stage-and-commit)
