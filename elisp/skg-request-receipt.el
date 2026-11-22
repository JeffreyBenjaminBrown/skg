;;; skg-request-receipt.el --- Write a read receipt for a mailed item  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-receipt' (C-c m a): write a read receipt for the item at
;;; point, in a view of someone's mailbox to you: the server writes a
;;; note, how you handled the item, as the first child of your fork of
;;; it (forking it first if you hold none), and hides the item from
;;; your fork of the mailbox, in one write, then redraws the views that
;;; show them. When the server needs a choice (the mailbox, where the
;;; item is in several; the skgrepo of a new fork, where nothing safe
;;; suggests one), it refuses, naming the choices;
;;; 'skg--receipt-handler' asks, and 'skg--receipt-send' sends again.
;;; Sending the request and receiving the redraws are
;;; 'skg-request-rerender-all-views.el's ('skg--send-streaming-request').
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Receipt", and
;;; docs/forks.org, "Read receipts".
;;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'skg-buffer)    ; for skg--unsaved-view-buffers
(require 'skg-metadata)  ; for skg--metadata-sexp-at-point-or-nil, skg--node-id
(require 'skg-request-rerender-all-views) ; for skg--send-streaming-request

(defconst skg-receipt-default-text "Read."
  "The receipt's text when you type none, and its title.")

(defun skg-receipt ()
  "Write a read receipt for the node at point: one line saying how you
handled it (\"Read.\" by default) goes under your fork of it, which is
made if you hold none, and the node is hidden from your fork of the
mailbox it came in. Refuses while this buffer has unsaved edits, which
the redraw would replace; other buffers with unsaved edits are left as
they are."
  (interactive)
  (unless skg-view-id
    (user-error "Not in a skg view"))
  (when (buffer-modified-p)
    (user-error "Save or revert this buffer first: the receipt redraws it, which would lose its unsaved edits"))
  (let ((item (save-excursion
                (org-back-to-heading t)
                (skg--node-id (skg--metadata-sexp-at-point-or-nil)))))
    (unless item
      (user-error "Point must be on a node's headline"))
    (skg--receipt-send
     item
     (read-string (format "Receipt (default %s): " skg-receipt-default-text)
                  nil nil skg-receipt-default-text)
     nil nil)))

(defun skg--receipt-send (item text mailbox fork-into)
  "Ask the server for a receipt for ITEM saying TEXT, hiding it from
MAILBOX (inferred when nil), forking it into FORK-INTO (the server's
choice when nil). The views with unsaved edits are not redrawn."
  (skg--send-streaming-request
   "receipt" 'receipt
   (append `((request . "receipt")
             (item . ,item)
             (text . ,text))
           (when mailbox `((mailbox . ,mailbox)))
           (when fork-into `((fork-into . ,fork-into)))
           `((exclude-view-ids
              ,@(mapcar (lambda (buffer) (buffer-local-value 'skg-view-id buffer))
                        (skg--unsaved-view-buffers)))))
   (lambda (_tcp-proc payload)
     (skg--receipt-handler item text mailbox fork-into payload))
   nil t))

(defun skg--receipt-handler (item text mailbox fork-into payload)
  "Show what the receipt PAYLOAD says it wrote. A refusal that names
choices asks for one once the empty rerender stream after it is done,
and sends the receipt for ITEM, TEXT, MAILBOX and FORK-INTO again with
it; any other refusal is shown."
  (let* ((response (read payload))
         (refusal (cadr (assq 'refusal response)))
         (strings (lambda (key)
                    (mapcar (lambda (x) (format "%s" x))
                            (cadr (assq key response))))))
    (if (not refusal)
        (message "%s" (mapconcat #'identity (funcall strings 'lines) "\n"))
      (let ((skgrepos (funcall strings 'choose-skgrepo))
            (mailboxes (funcall strings 'choose-mailbox))
            (titles (funcall strings 'choose-mailbox-titles)))
        (cond
         (skgrepos
          (setq skg--rerender-after-empty-stream
                (lambda ()
                  (skg--receipt-send
                   item text mailbox
                   (completing-read (format "%s\nFork into: " refusal)
                                    skgrepos nil t)))))
         (mailboxes
          (setq skg--rerender-after-empty-stream
                (lambda ()
                  (let* ((choices (cl-mapcar (lambda (pid title)
                                               (cons (format "%s (%s)" title pid) pid))
                                             mailboxes titles))
                         (chosen (completing-read
                                  (format "%s\nHide it from: " refusal)
                                  choices nil t)))
                    (skg--receipt-send item text
                                       (cdr (assoc chosen choices))
                                       fork-into)))))
         (t (message "skg receipt: %s" refusal)))))))

(provide 'skg-request-receipt)
