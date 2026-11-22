;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: What the client does when the server stops a save to ask.
;;; Each handler here answers one of the alternatives to save-result
;;; that `skg--send-save-buffer' registers: a fork-confirmation, a
;;; delete-from-confirmation, a strandeds-confirmation, a
;;; telescope-hoist-confirmation, or an
;;; overPrivateText-telescope-confirmation (text release). All but the
;;; last ask the user and re-issue the same save with the answer.

(require 'cl-lib)
(require 'skg-log)
(require 'skg-length-prefix) ; for skg-tell-answer-not-applied
(require 'skg-buffer)
(require 'skg-metadata)
(require 'skg-config)        ; for skg--owned-repos
;; skg-request-save requires this file, so these are defined by the
;; time any handler here runs.
(declare-function skg-request-save-buffer "skg-request-save"
                  (&optional approved-forks fork-skgrepos approved-hoist-pids
                             strandeds-review delete-from))
(declare-function skg--end-save-without-save-result "skg-request-save" ())
(declare-function skg-big-nonfatal-message "skg-request-save"
                  (buffer-name message-text content))
(declare-function skg--message-list-nonempty-p "skg-request-save"
                  (message-list))
(declare-function skg--save-hides-message "skg-request-save"
                  (hides &optional shown))
(declare-function skg--request-rerender-views "skg-request-rerender-all-views"
                  (view-ids approved-pids))

(defconst skg-fork-repo-placeholder "PICK-A-REPO"
  "Sentinel repo the server pre-fills for a clone-to-be whose repo
the user has not specified (in the saved metadata or a prior round).
`skg--fork-choose-placeholder-repos' prompts for a replacement per
carrying clone (the user can also set one with C-c s s);
`skg-approve-fork' refuses while any remains. Must match
FORK_REPO_PLACEHOLDER in server/from_text/fork.rs.")

(defvar-local skg--fork-source-buffer nil
  "In a fork-confirmation buffer, the source buffer whose save raised the
forks. `skg-approve-fork' re-saves it (approved); `skg-decline-fork'
leaves things untouched.")

(defvar-local skg--fork-suppress-strip-on-kill nil
  "When non-nil, `skg--fork-confirmation-on-kill' does not strip the
source's fork atom. `skg-approve-fork' sets it before killing this
buffer, because its re-save still needs the atom to skgsave-commit the fork.")

(defvar-local skg--fork-delete-from nil
  "In a fork-confirmation buffer, the delete-from choice the forking save
carried, which `skg-approve-fork' carries again.")

(defvar-local skg--fork-strandeds-review nil
  "In a fork-confirmation buffer, the strandeds review the forking save
carried, for `skg-approve-fork' to carry again.")

(defvar-local skg--pending-fork-result nil
  "On a fork's source buffer, while the approved re-save is pending, the
alist ((N . SKGREPO) ...) it carries: each forked node's ID and the
owned skgrepo its clone goes into.  The save-result handler reports the
save's outcome against them.")

(defun skg--fork-confirmation-on-kill ()
  "`kill-buffer-hook' for a fork-confirmation buffer: dismissing it
without approving strips the lingering (viewRequests fork) atom from the
source buffer, so the next save does not silently re-fork. This covers
killing the buffer directly (C-x k, q, etc.); `skg-decline-fork' already
strips explicitly, and `skg-approve-fork' suppresses this (its re-save
needs the atom, and the server drops it on re-render). Stripping is
idempotent, so a redundant call after a decline is a harmless no-op."
  (unless skg--fork-suppress-strip-on-kill
    (when (buffer-live-p skg--fork-source-buffer)
      (with-current-buffer skg--fork-source-buffer
        (skg-strip-fork-requests-in-buffer)))))

(defun skg--forks-phrase (fork-skgrepos)
  "Describe FORK-SKGREPOS, an alist ((N . SKGREPO) ...), as in
\"1 node into skgrepo A\" or \"2 nodes into skgrepos A, B\"."
  (let ((count (length fork-skgrepos))
        (skgrepos (delete-dups (mapcar #'cdr fork-skgrepos))))
    (format "%d %s into %s %s"
            count (if (= count 1) "node" "nodes")
            (if (cdr skgrepos) "skgrepos" "skgrepo")
            (string-join skgrepos ", "))))

(defun skg--close-fork-confirmation (confirmation source fork-skgrepos)
  "Close CONFIRMATION once its forks are approved, and record
FORK-SKGREPOS on SOURCE as its pending fork result.  Each window
showing CONFIRMATION is quit as by `quit-window': a window
`display-buffer' made for it is deleted, and one it borrowed shows
its earlier buffer again, so no split is left unexplained.  The echo
area says the save is under way."
  (dolist (window (get-buffer-window-list confirmation nil t))
    (when (window-live-p window)
      (quit-restore-window window)))
  (with-current-buffer source
    (setq skg--pending-fork-result fork-skgrepos))
  (let ((kill-buffer-query-functions nil))
    (kill-buffer confirmation))
  (message "Fork approved; saving %s..." (skg--forks-phrase fork-skgrepos)))

(defun skg--finish-pending-fork-result (save-buffer payload)
  "Report the outcome of SAVE-BUFFER's approved fork save, from its
save-result PAYLOAD.  Success goes to the echo area, followed by any
hides the save reports.  A failure says so there too; its errors, and
any warnings, are in the buffer `skg-handle-save-sexp' showed.  A result
that cannot be read gets a buffer of its own."
  (when (buffer-live-p save-buffer)
    (let ((fork-skgrepos
           (buffer-local-value 'skg--pending-fork-result save-buffer)))
      (when fork-skgrepos
        (with-current-buffer save-buffer
          (setq skg--pending-fork-result nil))
        (let ((forks (skg--forks-phrase fork-skgrepos)))
          (condition-case err
              (let* ((response (read payload))
                     (errors (cadr (assoc 'errors response)))
                     (warnings (cadr (assoc 'warnings response)))
                     (hides (cadr (assq 'hides response))))
                (if (skg--message-list-nonempty-p errors)
                    (message "Forking %s failed; the errors are shown in their own buffer."
                             forks)
                  (message "%s"
                           (skg--save-hides-message
                            hides
                            (format "Forked %s%s." forks
                                    (if (skg--message-list-nonempty-p warnings)
                                        ", with warnings (shown in their own buffer)"
                                      ""))))))
            (error
             (skg-big-nonfatal-message
              "*SKG Fork Result*"
              (format "Forking %s: could not read the save result." forks)
              (format "* Forking %s: could not read the save result.\n%S\n"
                      forks err)))))))))

(defun skg--fork-confirmation-handler (save-buffer payload &optional strandeds-review
                                                     delete-from)
  "Handle a `fork-confirmation' LP message: the save edited foreign
node(s) and was not pre-approved, so nothing was skgsave-committed. Show the
confirmation buffer (which lists the nodes that would be forked) and,
interactively, prompt for any clone repo not yet specified, then ask
whether to approve.

Terminal, like `skg--save-result-handler': remove the streaming handlers
and the unfired save-result one-shot (decrementing the pending count for
it), end the stream, and unlock. STRANDEDS-REVIEW and DELETE-FROM are the
review and the delete-from choice the save carried, which the approving
re-save carries again."
  (skg--end-save-without-save-result)
  (let ((confirm-buf
         ;; The condition-case guards only response parsing/display.
         ;; The interactive flow below runs outside it: a refusal
         ;; (user-error) from `skg-approve-fork' must reach the user,
         ;; not the log -- nesting it here used to swallow the
         ;; \"pick a repo first\" refusal, so approving with a
         ;; placeholder skgrepo silently did nothing.
         (condition-case err
             (let* ((response (read payload))
                    (content (cadr (assoc 'content response)))
                    (to-minibuffer (cadr (assoc 'to-minibuffer response)))
                    (buf (skg--show-fork-confirmation content save-buffer)))
               (with-current-buffer buf
                 (setq skg--fork-strandeds-review strandeds-review)
                 (setq skg--fork-delete-from delete-from))
               (when to-minibuffer (message "%s" to-minibuffer))
               buf)
           (error
            (skg-log 'error 'save "fork-confirmation handler error: %S" err)
            (skg-tell-answer-not-applied
             "fork-confirmation" err
             "nothing was saved, and the fork confirmation was not shown")
            nil))))
    (when (and confirm-buf (not noninteractive))
      ;; In batch (tests) the caller drives skg-approve-fork /
      ;; skg-decline-fork directly; interactively, ask now. Quitting
      ;; (C-g) any prompt leaves the confirmation buffer open: set
      ;; skgrepos with C-c s s and approve with C-c C-c, or decline
      ;; with C-c C-k.
      (with-current-buffer confirm-buf
        (skg--fork-choose-placeholder-repos)
        (if (yes-or-no-p "Fork the listed node(s)? ")
            (skg-approve-fork)
          (skg-decline-fork))))))

;;; Delete-from confirmation: under a skgrepo restriction, the save's deletes
;;; would strip records the skgrepo restriction keeps out. The user chooses
;;; whether those go too.

(defun skg--delete-from-confirmation-handler
    (save-buffer payload approved-forks fork-skgrepos
                 approved-hoist-pids strandeds-review)
  "Handle a `delete-from-confirmation' message for SAVE-BUFFER: records the
skgrepo restriction keeps out name a graphnode the save deletes, and
nothing was skgsave-committed. Ask \"Delete from: (c)ancel, (v)isible
records only, (a)ll owned records\", and re-issue the same save with the
choice, carrying the other approvals along. Cancel saves nothing."
  (skg--end-save-without-save-result)
  (condition-case err
      (let* ((response (read payload))
             (count (cadr (assoc 'count response)))
             (prompt (or (cadr (assoc 'prompt response))
                         "Records the skgrepo restriction keeps out name a deleted graphnode.")))
        (if noninteractive
            (message "%s restricted record(s) name a deleted graphnode; nothing was saved"
                     count)
          (let ((delete-from
                 (skg--delete-from-of-choice
                  (read-char-choice
                   (concat prompt
                           " Delete from: (c)ancel, (v)isible records only, (a)ll owned records ")
                   '(?c ?v ?a)))))
            (if (not delete-from)
                (message "Nothing was saved.")
              (with-current-buffer save-buffer
                (skg-request-save-buffer
                 approved-forks fork-skgrepos approved-hoist-pids
                 strandeds-review delete-from))))))
    (error
     (skg-log 'error 'save "delete-from-confirmation handler error: %S" err)
     (skg-tell-answer-not-applied
      "delete-from-confirmation" err
      "the save stopped for a confirmation, which failed, so it may not have saved"))))

(defun skg--delete-from-of-choice (choice)
  "The delete-from field's value for CHOICE, a key of the delete-from
prompt: \"visible\" for ?v, \"all\" for ?a, nil (cancel) otherwise."
  (pcase choice
    (?v "visible")
    (?a "all")
    (_ nil)))

;;; Strandeds confirmation: a home skgrepo move would leave relationships at their
;;; old, more private skgrepos. The server decides which
;;; (server/from_text/strandeds.rs); the user chooses which to reset to default.

(defun skg--strandeds-confirmation-handler
    (save-buffer payload approved-forks fork-skgrepos
                 approved-hoist-pids &optional delete-from)
  "Handle a `strandeds-confirmation' message for SAVE-BUFFER: the save's home
skgrepo moves would strand the relationships it lists, and nothing was
skgsave-committed. Show them, ask which to reset to default, and re-issue the
same save with that review, carrying the other approvals along."
  (skg--end-save-without-save-result)
  (condition-case err
      (let* ((response (read payload))
             (strandeds (skg--strandeds-from-response response))
             (prompt (or (cadr (assoc 'prompt response))
                         "Reset the stranded relationships to default? ")))
        (if noninteractive
            (message "%d relationship(s) would be stranded; nothing was saved"
                     (length strandeds))
          (skg--show-strandeds strandeds)
          (let ((review (skg--strandeds-review-from-choice
                         strandeds
                         (car (read-multiple-choice
                               prompt
                               '((?a "all" "reset every listed relationship to default")
                                 (?n "none" "save, leaving them where they are")
                                 (?c "choose" "ask about each")
                                 (?q "quit" "save nothing"))))
                         #'y-or-n-p)))
            (if (not review)
                (message "Nothing was saved.")
              (with-current-buffer save-buffer
                (skg-request-save-buffer
                 approved-forks fork-skgrepos approved-hoist-pids
                 review delete-from))))))
    (error
     (skg-log 'error 'save "strandeds-confirmation handler error: %S" err)
     (skg-tell-answer-not-applied
      "strandeds-confirmation" err
      "the save stopped for a confirmation, which failed, so it may not have saved"))))

(defun skg--strandeds-from-response (response)
  "The stranded relationships in a strandeds-confirmation RESPONSE, each a
plist (:relation :recorder :member :relRepo :default) of strings."
  (mapcar (lambda (entry)
            (let ((field (lambda (key)
                           (format "%s" (cadr (assoc key entry))))))
              (list :relation (funcall field 'relation)
                    :recorder (funcall field 'recorder)
                    :member   (funcall field 'member)
                    :relRepo  (funcall field 'relRepo)
                    :default  (funcall field 'default))))
          (cadr (assoc 'strandeds response))))

(defun skg--stranded-description (stranded)
  "One line describing STRANDED, a plist from `skg--strandeds-from-response'."
  (format "%s %s %s: stays at %s; its default is now %s"
          (plist-get stranded :recorder) (plist-get stranded :relation)
          (plist-get stranded :member) (plist-get stranded :relRepo)
          (plist-get stranded :default)))

(defun skg--show-strandeds (strandeds)
  "List STRANDEDS in the *SKG Stranded Relationships* buffer."
  (with-current-buffer (get-buffer-create "*SKG Stranded Relationships*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "These relationships would stay in their old, more private skgrepos:\n")
      (dolist (stranded strandeds)
        (insert "- " (skg--stranded-description stranded) "\n"))
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

(defun skg--strandeds-review-from-choice (strandeds choice ask)
  "The strandeds review CHOICE (a key from the strandeds prompt) makes of
STRANDEDS: (reviewed . TRIPLES), each TRIPLE (RELATION RECORDER MEMBER)
a relationship to reset to default; or nil, to save nothing. For ?c, ASK is
called with a prompt per relationship, and resets to default those it accepts."
  (let ((triple (lambda (stranded)
                  (list (plist-get stranded :relation)
                        (plist-get stranded :recorder)
                        (plist-get stranded :member)))))
    (pcase choice
      (?a (cons 'reviewed (mapcar triple strandeds)))
      (?n (list 'reviewed))
      (?c (cons 'reviewed
                (delq nil
                      (mapcar (lambda (stranded)
                                (when (funcall ask
                                               (format "Reset %s to default? "
                                                       (skg--stranded-description
                                                        stranded)))
                                  (funcall triple stranded)))
                              strandeds))))
      (_ nil))))

(defun skg--telescope-hoist-confirmation-handler
    (save-buffer payload approved-forks fork-skgrepos
                 &optional strandeds-review delete-from)
  "Handle the text-free terminal Hoist challenge for SAVE-BUFFER.
The server has skgsave-committed nothing.  On approval, reissue the same save with
the exact candidate PIDs; on Abort, leave the buffer and every .skg file
untouched.  FORK-APPROVED and FORK-REPOS survive if this challenge arose
on a retry that had already received fork authority, STRANDEDS-REVIEW
if it had already been reviewed, and DELETE-FROM if it had been chosen."
  (skg--end-save-without-save-result)
  (condition-case err
      (let* ((response (read payload))
             (telescopes (cadr (assoc 'telescopes response)))
             (approved-pids
              (mapcar (lambda (entry)
                        (format "%s" (cadr (assoc 'pid entry))))
                      telescopes))
             (prompt (or (cadr (assoc 'prompt response))
                         "Hoist lower title/body text to home? ")))
        (if noninteractive
            (message "Hoist required for %s; nothing was saved"
                     (mapconcat #'identity approved-pids ", "))
          (if (yes-or-no-p prompt)
              (with-current-buffer save-buffer
                (skg-request-save-buffer
                 approved-forks fork-skgrepos approved-pids
                 strandeds-review delete-from))
            (message
             "Hoist aborted; nothing was saved. Repair the .skg sections manually."))))
    (error
     (skg-log 'error 'save
              "telescope-hoist-confirmation handler error: %S" err)
     (skg-tell-answer-not-applied
      "telescope-hoist-confirmation" err
      "the save stopped for a confirmation, which failed, so it may not have saved"))))

(defun skg--save-text-release-confirmation-handler (payload)
  "Handle a save-rerender text release challenge, the terminal message
of a save whose rerender would show text from overPrivateText
telescopes under the skgrepo restriction. The save has been written,
but the server has sent none of the rerendered text; its challenge
names the views withheld, the saved one first. Approval asks the
server to rerender those views, approving the PIDs the challenge
lists (`skg--request-rerender-views'); the save is not sent again,
since it would apply a second time (creating a new root of the view
again, or refused because a container it gave a new child would be an
edited stale graphnode).
Declining leaves the buffers as they are."
  (skg--end-save-without-save-result)
  (condition-case err
      (let* ((response (read payload))
             (approved-pids
              (mapcar (lambda (pid) (format "%s" pid))
                      (cadr (assoc 'pids response))))
             (withheld-view-ids
              (mapcar (lambda (view-id) (format "%s" view-id))
                      (cadr (assoc 'view-ids response))))
             (prompt (or (cadr (assoc 'prompt response))
                         "Display staged text from overPrivateText telescopes? ")))
        (if noninteractive
            (message "Saved, but protected rerender text was withheld for %s"
                     (mapconcat #'identity approved-pids ", "))
          (if (yes-or-no-p prompt)
              (skg--request-rerender-views withheld-view-ids approved-pids)
            (message
             "Save succeeded; protected rerender text remains withheld and buffers are unchanged."))))
    (error
     (skg-log 'error 'save
              "save text-release confirmation handler error: %S" err)
     (skg-tell-answer-not-applied
      "overPrivateText-telescope-confirmation" err
      "the server saved, but the views it withheld were not redrawn"))))

(defun skg--fork-suggested-repo-above-point ()
  "Return the suggested repo named by the comment directly above the
headline at point, or nil. The server writes that comment above each
clone-to-be whose repo the user has not yet specified."
  (save-excursion
    (forward-line -1)
    (when (looking-at
           "^# Suggested repo for the clone below: \\(.+\\)$")
      (string-trim (match-string 1)))))

(defun skg--fork-choose-placeholder-repos ()
  "Prompt for an owned repo for each clone-to-be still carrying
`skg-fork-repo-placeholder', writing the choice into the buffer.
The server's suggested repo (the comment above the clone) is the
default; S-left/S-right cycle through the repos you own. A no-op
when every clone's repo is already specified -- notably when the
saved metadata itself specified it (the server then omits the
placeholder): no redundant ask."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (beginning-of-line)
      (let ((sexp (skg--metadata-sexp-at-point-or-nil)))
        (when (and (= (org-current-level) 1)
                   sexp
                   (equal (skg--node-repo sexp)
                          skg-fork-repo-placeholder))
          (let* ((title (nth 2 (skg-split-as-stars-metadata-title
                                (skg-get-current-headline-text))))
                 (suggested (skg--fork-suggested-repo-above-point))
                 (owned (skg--owned-repos))
                 (default (or suggested (car owned)))
                 (choice (skg--completing-read-with-cycle
                          (format "Repo for the clone \"%s\" (default %s): "
                                  title default)
                          owned nil t nil nil default nil owned)))
            (skg--change-repo-at-point
             (if (string-empty-p choice) default choice)))))
      (forward-line 1))))

(defun skg--show-fork-confirmation (content save-buffer)
  "Show CONTENT in the *SKG Fork Confirmation* buffer, recording
SAVE-BUFFER as its source, and return the buffer. The buffer is a
navigable content view (so id-push / search work). It is editable so
each clone's repo can be set (the handler's minibuffer prompts write
into it; C-c s s on a clone-to-be works too), but it is not an ordinary
save target: skg-view-id is left nil (tripping the nil-view-id save
guard) and C-x C-s is rebound to refuse, because a stray normal save of
its id-less clone-to-be parents would create bare nodes. Only C-c C-c
\(approve) and C-c C-k (decline) act on it."
  (let ((buf (get-buffer-create "*SKG Fork Confirmation*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content ""))
        (skg-content-view-mode)
        (setq mode-name ;; not "SKG": this is not a savable view
              "skg-fork-ask")
        (when (fboundp 'heralds-minor-mode) (heralds-minor-mode))
        (goto-char (point-min)))
      ;; nil view-id: not a registered view, and the ordinary-save guard
      ;; rejects M-x skg-request-save-buffer on it.
      (setq skg-view-id nil)
      (setq skg--fork-source-buffer save-buffer)
      (setq skg--fork-suppress-strip-on-kill nil)
      ;; Dismissing this buffer without approving (killing it directly,
      ;; not via C-c C-k) must still strip the source's fork atom.
      (add-hook 'kill-buffer-hook #'skg--fork-confirmation-on-kill nil t)
      ;; Copy the mode map first so these overrides stay buffer-local --
      ;; local-set-key mutates (current-local-map) in place, which is the
      ;; shared skg-content-view-mode-map; rebinding C-x C-s on the shared
      ;; map would break saving in every content view.
      (use-local-map (copy-keymap (current-local-map)))
      ;; C-c C-c skgsave-commits the forks; C-c C-k declines; C-x C-s refuses
      ;; (this buffer must not be saved as ordinary content).
      (local-set-key (kbd "C-c C-c") #'skg-approve-fork)
      (local-set-key (kbd "C-c C-k") #'skg-decline-fork)
      (local-set-key (kbd "C-x C-s") #'skg--fork-confirmation-refuse-save)
      (set-buffer-modified-p nil))
    (display-buffer buf)
    buf))

(defun skg--fork-confirmation-refuse-save ()
  "Refuse an ordinary save of the fork-confirmation buffer.
Its clone-to-be parents are id-less owned-repo nodes; saving them as
ordinary content would create bare nodes. Approve with C-c C-c (skgsave-commits
the forks) or decline with C-c C-k."
  (interactive)
  (user-error
   "This is the fork-confirmation buffer; use C-c C-c to approve or C-c C-k to decline"))

(defun skg--fork-repos-from-confirmation-buffer ()
  "Walk the fork-confirmation buffer; return an alist ((N . REPO) ...).
Each level-1 headline is a clone-to-be carrying (skgRepos (nodeRepo
REPO)) and no gnid; each of its level-2 children is an original
carrying (gnid N). The
clone's REPO (rotated by the user, or the default) is paired with each
child's id N -- the key by which the server applies the chosen repo."
  (let ((pairs nil)
        (parent-skgrepo nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (let ((level (org-current-level))
              (sexp (skg--metadata-sexp-at-point-or-nil)))
          (cond
           ((= level 1)
            ;; Always rebind on a level-1 headline -- even a
            ;; metadata-less or garbled one -- so it cannot leak a prior
            ;; clone-to-be's repo to a later fork's child.
            (setq parent-skgrepo (and sexp (skg--node-repo sexp))))
           ((and (= level 2) sexp parent-skgrepo)
            (let ((gnid (skg--node-id sexp)))
              (when gnid
                (push (cons gnid parent-skgrepo) pairs))))))
        (forward-line 1)))
    (nreverse pairs)))

(defun skg-approve-fork ()
  "Approve the fork operations listed in this *SKG Fork Confirmation* buffer:
extract each clone's chosen repo, close the confirmation, and re-save
the source buffer with the fork operations approved (carrying those
repos).  When the approved save finishes, the echo area says where the
forks went, or that the save failed
\(`skg--finish-pending-fork-result').

Interactively, first prompts for any clone repo still at
`skg-fork-repo-placeholder' (as the confirmation handler already did
when the buffer appeared -- this catches placeholders that survived,
e.g. after quitting those prompts). Refuses if any placeholder remains
anyway; the confirmation buffer is left open so you can set repos by
hand (C-c s s)."
  (interactive)
  (unless noninteractive
    (skg--fork-choose-placeholder-repos))
  (let ((source skg--fork-source-buffer)
        (strandeds-review skg--fork-strandeds-review)
        (delete-from skg--fork-delete-from)
        (fork-skgrepos (skg--fork-repos-from-confirmation-buffer)))
    (unless (buffer-live-p source)
      (error "The buffer that requested these forks is no longer open"))
    (when (seq-some (lambda (pair)
                      (string= (cdr pair) skg-fork-repo-placeholder))
                    fork-skgrepos)
      (user-error
       "Pick a repo for each clone first: point on a clone-to-be headline, then C-c s s"))
    ;; The atom must survive to the re-save (which skgsave-commits the fork; the
    ;; server then drops it on re-render), so suppress the kill-hook strip.
    (setq skg--fork-suppress-strip-on-kill t)
    (skg--close-fork-confirmation (current-buffer) source fork-skgrepos)
    (with-current-buffer source
      (skg-request-save-buffer t fork-skgrepos nil strandeds-review
                               delete-from))))

(defun skg-decline-fork ()
  "Decline the forks; nothing was written. Strip any lingering explicit
\(viewRequests fork) atom from the source buffer -- otherwise the next save
of that buffer would silently re-fork -- then leave this confirmation
buffer open (it is navigable -- search it for relevant IDs).

Stripping is a no-op for an implicit (foreign) fork, whose source headline
carries no fork atom."
  (interactive)
  (when (buffer-live-p skg--fork-source-buffer)
    (with-current-buffer skg--fork-source-buffer
      (skg-strip-fork-requests-in-buffer)))
  (message
   "Fork declined; nothing was saved. This buffer is left open for reference."))

(provide 'skg-save-confirmations)
