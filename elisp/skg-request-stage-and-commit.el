;;; skg-request-stage-and-commit.el --- Git-commit the changed nodes  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Git-commit the changed nodes of the owned skgrepos, with one
;;; message ('docs/diff-view.org', "Stage and commit"):
;;; 'skg-stage-and-commit' (C-c r c) commits every node whose changes
;;; are all processed, except one that needs a node whose changes are
;;; not; 'skg-stage-and-commit-everything' (C-c r C) commits every
;;; changed node. Each refuses while a view has unsaved edits, then
;;; opens the commit-message buffer
;;; ('skg--stage-and-commit-open-message-buffer'), as in magit:
;;; 'skg-commit-message-send' (C-c C-c) sends it and
;;; 'skg-commit-message-cancel' (C-c C-k) cancels. The buffer remembers
;;; which of the two commands opened it last
;;; ('skg--commit-processed-only'), and keeps the message until it is
;;; moot ('skg--stage-and-commit-report-result').
;;; 'skg--stage-and-commit-result-message' words the answer. Marking
;;; changes processed is 'skg-processed.el's. Its Neovim twin is
;;; 'nvim/lua/skg/stage_and_commit.lua'.
;;;
;;; A commit of everything while some changed node is not processed
;;; makes the server ask for confirmation instead, changing nothing;
;;; only the typed word "yes" resends the request confirmed. A commit
;;; of the processed asks nothing. Every answer is followed by a
;;; rerender stream ('skg--send-streaming-request'), which after a
;;; commit rerenders the open buffers of the diff view.
;;;
;;; DATA USED/ASSUMED: See 'docs/api-and-formats.org', "Stage and
;;; commit".
;;;
;;; Code:

(require 'skg-buffer)
(require 'skg-processed) ; for skg--processed-summary-message
(require 'skg-request-rerender-all-views)

(defconst skg--commit-message-buffer-name "*skg commit message*"
  "The buffer in which `skg-stage-and-commit' takes the commit message.")

(defvar-local skg--commit-processed-only t
  "In the commit-message buffer: non-nil when C-c C-c commits only the
processed nodes (`skg-stage-and-commit'), nil when it commits every
changed node (`skg-stage-and-commit-everything').")

(defvar skg-commit-message-mode-map (make-sparse-keymap)
  "Keymap for `skg-commit-message-mode'.")

(let ((map skg-commit-message-mode-map))
  (setcdr map nil)
  (define-key map (kbd "C-c C-c") #'skg-commit-message-send)
  (define-key map (kbd "C-c C-k") #'skg-commit-message-cancel))

(define-derived-mode skg-commit-message-mode text-mode "skg-commit-message"
  "The buffer in which `skg-stage-and-commit' and
`skg-stage-and-commit-everything' take the commit message.
\\<skg-commit-message-mode-map>\\[skg-commit-message-send] commits, with the buffer's text as the message;
\\[skg-commit-message-cancel] cancels.")

(defun skg-stage-and-commit ()
  "C-c r c commits every node whose changes are all processed, except
one that needs a node whose changes are not. The message is written
in the buffer `skg--commit-message-buffer-name' (see
`skg-commit-message-mode'), where a message left by an earlier attempt
is kept. Nothing is asked: the line shown afterward says how many
nodes went in, how many changed nodes are left, and which of those
are processed but wait, and for what. `skg-stage-and-commit-everything'
commits the rest."
  (interactive)
  (skg--stage-and-commit-open-message-buffer t))

(defun skg-stage-and-commit-everything ()
  "Commit every changed node of the owned skgrepos, processed or not,
with a message written as for `skg-stage-and-commit'. If some changed
node is not processed, show the processed summary first, and commit
only if the user types \"yes\"."
  (interactive)
  (skg--stage-and-commit-open-message-buffer nil))

(defun skg--stage-and-commit-open-message-buffer (processed-only)
  "Show the commit-message buffer, creating it if there is none, and
make its C-c C-c commit the processed nodes when PROCESSED-ONLY, or
every changed node otherwise."
  (skg--stage-and-commit-require-saved-views)
  (let ((buffer (get-buffer-create skg--commit-message-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'skg-commit-message-mode)
        (skg-commit-message-mode))
      (setq skg--commit-processed-only processed-only)
      (setq header-line-format
            (substitute-command-keys
             (format "\\<skg-commit-message-mode-map>Commit message: \\[skg-commit-message-send] commits %s, \\[skg-commit-message-cancel] cancels"
                     (if processed-only "the processed nodes"
                       "every changed node")))))
    (pop-to-buffer buffer)))

(defun skg-commit-message-send ()
  "Commit with this buffer's text as the commit message, less its
trailing whitespace: the processed nodes, or every changed node,
whichever command opened the buffer last. Then hide the buffer,
restoring its window. The buffer is killed once the message is moot
\(`skg--stage-and-commit-report-result'); until then it keeps the text,
for the next attempt. A blank message is refused."
  (interactive)
  (skg--commit-message-require-mode)
  (let ((commit-message
         (string-trim-right
          (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p commit-message)
      (user-error "A commit message is required"))
    (skg--stage-and-commit-require-saved-views)
    (skg--stage-and-commit-send commit-message skg--commit-processed-only nil)
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

(defun skg--stage-and-commit-send (commit-message processed-only confirmed)
  "Send the request with COMMIT-MESSAGE, with '(processed-only . t)'
when PROCESSED-ONLY, and '(confirmed . t)' when CONFIRMED. The message
goes one string per line, as '(message-lines \"LINE\" ...)', since the
server reads one request per line."
  (skg--send-streaming-request
   "stage and commit"
   'stage-and-commit
   (append `((request . "stage and commit")
             (message-lines ,@(split-string commit-message "\n")))
           (when processed-only '((processed-only . t)))
           (when confirmed '((confirmed . t))))
   (lambda (_tcp payload)
     (skg--stage-and-commit-handler commit-message payload))
   nil t))

(defun skg--stage-and-commit-handler (commit-message payload)
  "Handle the stage-and-commit response PAYLOAD. A request for
confirmation, which only a commit of everything gets, is asked about
once the empty rerender stream that follows is done, and \"yes\"
resends COMMIT-MESSAGE confirmed."
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
or nothing had changed and nothing failed, the commit message is moot
and its buffer is killed; otherwise (nothing processed, everything
processed waiting, a refusal, an error) the buffer keeps it, and the
line says so."
  (let ((message-moot
         (or (cadr (assq 'commits response))
             (and (null (skg--response-errors-line response))
                  (= 0 (or (cadr (assq 'left-nodes response)) 0))))))
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

(defun skg--stage-and-commit-confirm-and-resend (commit-message response)
  "Ask to confirm RESPONSE, a request for confirmation, and on \"yes\"
resend COMMIT-MESSAGE as a commit of everything, confirmed. Otherwise,
a quit at the prompt included, say that nothing was committed, and
where the message is kept."
  (if (condition-case nil
          (skg--stage-and-commit-confirmed-p response)
        (quit nil))
      (skg--stage-and-commit-send commit-message nil t)
    (let ((kept (skg--commit-message-kept-sentence "your")))
      (message "%s" (if (string-empty-p kept) "Not committed."
                      (concat "Not committed; " kept))))))

(defun skg--stage-and-commit-confirmed-p (response)
  "Show the processed summary of RESPONSE, a request for confirmation;
non-nil iff the user types exactly \"yes\"."
  (string= "yes"
           (read-string
            (format "%s. Type yes to commit anyway: "
                    (string-remove-suffix
                     "." (skg--processed-summary-message
                          (assq 'processed-summary response)))))))

(defun skg--count-phrase (count noun)
  "COUNT and NOUN, made plural by an \"s\" unless COUNT is 1."
  (format "%d %s%s" count noun (if (eql count 1) "" "s")))

(defun skg--stage-and-commit-result-message (response)
  "The line shown for RESPONSE, a result, '((commits ((SKGREPO
COMMIT-ID) ...)) (committed-nodes N) (left-nodes L) (waiting ((PID
TITLE REASON PID TITLE) ...)) (unmarkable ((PID TITLE (CHANGE ...))
...)) (errors (...)) (warnings (...)))': what went in, how many
changed nodes are left, for each processed node that waits, what it
waits for, and for each node left because it changed in a way no mark
covers, how."
  (let* ((commits (cadr (assq 'commits response)))
         (committed (cadr (assq 'committed-nodes response)))
         (left (or (cadr (assq 'left-nodes response)) 0))
         (waiting (cadr (assq 'waiting response)))
         (unmarkable (cadr (assq 'unmarkable response)))
         (errors-line (skg--response-errors-line response)))
    (concat
     (cond (commits
            (format "Committed %sin %s."
                    (if committed
                        (concat (skg--count-phrase committed "node") ", ")
                      "")
                    (mapconcat
                     (lambda (commit)
                       (let ((commit-id (format "%s" (cadr commit))))
                         (format "%s (%s)" (car commit)
                                 (substring commit-id 0 (min 7 (length commit-id))))))
                     commits ", ")))
           ((or errors-line waiting unmarkable) "Nothing was committed.")
           ((= left 0) "Nothing has changed, so nothing was committed.")
           (t "Nothing is processed, so nothing was committed; C-c r C commits everything."))
     (if (and (> left 0) (or commits waiting unmarkable))
         (concat " "
                 (skg--count-phrase left "changed node")
                 (if (= left 1) " is left" " are left")
                 (if waiting
                     (format "; %d of them %s: %s"
                             (length waiting)
                             (if (= 1 (length waiting))
                                 "is processed but waits"
                               "are processed but wait")
                             (mapconcat #'skg--stage-and-commit-waiting-clause
                                        waiting "; "))
                   "")
                 (if unmarkable
                     (format "; %d of them changed in %s no mark covers, which C-c r C or Git can commit: %s"
                             (length unmarkable)
                             (if (= 1 (length unmarkable)) "a way" "ways")
                             (mapconcat #'skg--stage-and-commit-unmarkable-clause
                                        unmarkable "; "))
                   "")
                 ".")
       "")
     (if errors-line (format " Errors: %s" errors-line) ""))))

(defun skg--stage-and-commit-waiting-clause (entry)
  "What ENTRY, '(PID TITLE REASON PID TITLE)' from a result's
'waiting', waits for, and why."
  (let ((title (nth 1 entry))
        (reason (nth 2 entry))
        (other-title (nth 4 entry)))
    (pcase reason
      ('names-new
       (format "'%s' names '%s', which is new and does not go in"
               title other-title))
      ('still-named
       (format "'%s' is deleted, but '%s', which does not go in, still names it"
               title other-title))
      ('merge
       (format "'%s' is part of a merge with '%s', which does not go in"
               title other-title))
      (_ (format "'%s' waits for '%s' (%s)" title other-title reason)))))

(defun skg--stage-and-commit-unmarkable-clause (entry)
  "ENTRY, '(PID TITLE (CHANGE ...))' from a result's 'unmarkable': the
node's title, and how it changed, in the server's words."
  (format "'%s' (%s)" (nth 1 entry) (mapconcat #'identity (nth 2 entry) ", ")))

(provide 'skg-request-stage-and-commit)
