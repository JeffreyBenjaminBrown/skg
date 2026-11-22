;;; skg-search-make-link.el --- Search, then insert a link to a hit  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-search-make-link' (C-c f l) searches for text and inserts a
;;; link to one of the results. Run from any buffer, typically a
;;; content view, it records the buffer object and point and runs the
;;; same search as C-c f RET ('skg-search'). In the search buffer
;;; 'skg-search-make-link-mode' is on; its C-c C-c
;;; ('skg-search-make-link-finish') picks the result at point, kills
;;; the search buffer, returns to the source buffer at the saved
;;; position and inserts [[id:X][LABEL]] there. Aside from C-c C-c
;;; the search buffer is like any other. The search itself is
;;; 'skg-request-text-search.el's, which runs this file's
;;; 'skg--maybe-activate-link-from-search-mode' through
;;; 'skg--search-buffer-setup-hook'.
;;;
;;; The label is the result's title with any inline [[id:Y][L]] links
;;; reduced to their labels (so the new link's label is plain text,
;;; never nested links), by 'skg-replace-links-with-labels'
;;; ('skg-id-search.el'). The user may save the source buffer
;;; afterwards (C-x C-s).
;;;
;;; PITFALL: do not save the source buffer eagerly, before the
;;; search. The server's rerender of the saved buffer can collapse a
;;; freshly-typed blank line into nothing, leaving the recorded
;;; integer point dangling on the next headline, so the inserted link
;;; glues itself to the wrong spot. Nothing forbids detours through
;;; the search results' links; they just operate against an unsaved
;;; source buffer.
;;;
;;; Code:

(require 'skg-id-search)
;; NOTE: deliberately not requiring skg-request-text-search here.
;; skg-request-text-search requires skg-client, and skg-client requires
;; this file — a real require cycle. The symbols we touch from there
;; (`skg-search', `skg--search-buffer-setup-hook') are referenced only
;; at runtime, not load time, and the hook lookup tolerates an
;; as-yet-undefined hook variable.

(defvar skg--link-from-search-pending nil
  "When non-nil, the next search buffer that opens
should activate `skg-search-make-link-mode'.
Holds (BUFFER . POSITION). BUFFER is the buffer object (not a
file path) because skg content view buffers are not file-visiting.
Cleared by `skg--maybe-activate-link-from-search-mode' once consumed.")

(defvar-local skg--link-from-search-target nil
  "Buffer-local: where this search buffer should insert its picked link.
A (BUFFER . POSITION) cons set when `skg-search-make-link-mode'
is turned on by `skg--maybe-activate-link-from-search-mode'.")

(defvar skg-search-make-link-mode-map (make-sparse-keymap)
  "Keymap for `skg-search-make-link-mode'.")

(let ((map skg-search-make-link-mode-map))
  (setcdr map nil)
  (define-key map (kbd "C-c C-c") #'skg-search-make-link-finish))

(define-minor-mode skg-search-make-link-mode
  "Search-buffer mode for picking a result and inserting a link.
Active only in search buffers opened by `skg-search-make-link'.
The active binding is C-c C-c, which calls
`skg-search-make-link-finish'."
  :keymap skg-search-make-link-mode-map
  :lighter " skg-link")

(defun skg-search-make-link (search-terms)
  "Search; then pick a result with C-c C-c to insert a link here.
SEARCH-TERMS is the same prompt as `skg-search'. On C-c C-c
in the search buffer, a [[id:X][LABEL]] link is inserted in
this buffer at the position point currently has."
  (interactive "sSearch terms (for link insertion): ")
  ;; Track the buffer object, not a file path: skg content view
  ;; buffers are not file-visiting (their text comes from the
  ;; server, not from disk).
  ;;
  ;; PITFALL: do not call `skg-request-save-buffer' here. See the
  ;; commentary at the top of this file.
  (setq skg--link-from-search-pending
        (cons (current-buffer) (point)))
  (skg-search search-terms))

(defun skg--maybe-activate-link-from-search-mode ()
  "If a `skg-search-make-link' was pending, configure this buffer for it.
Called from `skg--display-search-results' immediately after the
search buffer is set up. Sets the buffer-local target and turns
on `skg-search-make-link-mode'; clears the pending var so a later
plain search doesn't accidentally inherit the state."
  (when skg--link-from-search-pending
    (setq skg--link-from-search-target skg--link-from-search-pending)
    (skg-search-make-link-mode 1)
    (setq skg--link-from-search-pending nil)
    (message
     (substitute-command-keys
      "Pick a result with \\[skg-search-make-link-finish] to insert a link"))))

(defun skg-search-make-link-finish ()
  "Pick the result at point; insert a link in the source buffer.
Bound to C-c C-c by `skg-search-make-link-mode'. Uses
`skg-nearest-id' to identify the chosen ID and label;
`skg-replace-links-with-labels' normalises the label so it
never contains nested links. Then kills the search buffer
and inserts [[id:ID][LABEL]] at the saved buffer:position."
  (interactive)
  (unless skg--link-from-search-target
    (user-error "Not in a link-creation search buffer" ))
  (let ((found (skg-nearest-id)))
    (unless (consp found)
      ;; nil, or `ambiguous' (unreachable here -- a search buffer is
      ;; never in skg-file-minor-mode -- but never a valid pick).
      (user-error "No ID near point" ))
    (let* ((gnid     (car found))
           (raw    (cdr found))
           (label  (skg-replace-links-with-labels raw))
           (target skg--link-from-search-target)
           (dest-buf (car target))
           (pos      (cdr target))
           (search-buf (current-buffer)))
      (unless (buffer-live-p dest-buf)
        (user-error
         "Cannot insert link: source buffer no longer exists"))
      (kill-buffer search-buf)
      (switch-to-buffer dest-buf)
      (goto-char (min pos (point-max)))
      (insert (format "[[id:%s][%s]]" gnid label))
      (message "Inserted link to %s" gnid))))

(add-hook 'skg--search-buffer-setup-hook
          #'skg--maybe-activate-link-from-search-mode)

(provide 'skg-search-make-link)
