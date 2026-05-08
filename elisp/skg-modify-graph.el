;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: User-facing commands that modify graph structure.

(require 'org)
(require 'org-fold-core)
(require 'skg-config)
(require 'skg-metadata)

(defconst skg--headline-org-link-regex
  "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
  "Regex matching an org bracket link in a headline title.")

(defun skg-replace-content-with-link ()
  "Replace the branch at point with a link to its former root.
Point may be on the headline or in its body.  The root must be an
existing TrueNode with an ID.  Its org-parent must be a definitive
TrueNode whose source is owned by the user.  The whole org subtree
at point is replaced by a same-level headline whose title is an
org id link to the former root, then the buffer is saved."
  (interactive)
  (org-back-to-heading t)
  (let* ((node (skg--content-link-replacement-node-data))
         (container (skg--content-link-replacement-container-data)))
    (skg--check-content-link-replacement-container container)
    (when (and (skg--headline-title-has-link-p (plist-get node :title))
               (not (y-or-n-p "Are you sure? ")))
      (user-error "Canceled"))
    (skg--replace-current-subtree-with-link-headline
     (plist-get node :id)
     (plist-get node :title))
    (skg-request-save-buffer)))

(defun skg-replace-link-with-content ()
  "Replace the leaf at point with content linked from that leaf.
Point may be on the headline or in the body.  The leaf must have
exactly one org bracket link in its title plus body, no
org-descendents, and a definitive TrueNode org-parent whose source
is owned by the user.  The link must be an id link.  The leaf is
replaced by an indefinitive same-level TrueNode for the link
target, then the buffer is saved."
  (interactive)
  (org-back-to-heading t)
  (let* ((parent (skg--content-link-replacement-container-data))
         (node-id (skg--node-id
                   (skg--metadata-sexp-at-point-or-nil)))
         (link (skg--single-link-in-current-leaf)))
    (skg--check-content-link-replacement-container parent)
    (when node-id
      (message "Warning: replacing existing node %s may have created an orphan"
               node-id))
    (skg--replace-current-leaf-with-linked-content link)
    (skg-request-save-buffer)))

(defun skg--content-link-replacement-node-data ()
  "Return plist data for the TrueNode headline at point."
  (let* ((headline-text (skg-get-current-headline-text))
         (split (skg-split-as-stars-metadata-title headline-text))
         (metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
    (unless (skg--truenode-sexp-p metadata-sexp)
      (user-error "Cannot replace this branch with a link: it is not a truenode"))
    (let ((id (skg--node-id metadata-sexp)))
      (unless id
        (user-error "Cannot replace this branch with a link: node has no ID"))
      (list :id id
            :title (nth 2 split)))))

(defun skg--content-link-replacement-container-data ()
  "Return metadata for the org-parent of the headline at point."
  (save-excursion
    (unless (org-up-heading-safe)
      (user-error "Cannot replace this branch with a link: node has no container"))
    (let ((metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
      (unless (skg--truenode-sexp-p metadata-sexp)
        (user-error "Cannot replace this branch with a link: container is not a truenode"))
      metadata-sexp)))

(defun skg--check-content-link-replacement-container (metadata-sexp)
  "Signal a user error if METADATA-SEXP is not an editable container."
  (let ((source (skg--node-source metadata-sexp)))
    (unless source
      (user-error "Cannot replace this branch with a link: container has no source"))
    (unless (member source (skg--owned-sources))
      (user-error "Cannot replace this branch with a link: container source is not owned: %s"
                  source))
    (when (skg--node-indefinitive-p metadata-sexp)
      (user-error "Cannot replace this branch with a link: container is indefinitive"))))

(defun skg--replace-current-subtree-with-link-headline (id title)
  "Replace the current org subtree with a headline linking to ID.
The link label is TITLE with any nested links reduced to plain text."
  (let* ((stars (nth 0 (skg-split-as-stars-metadata-title
                       (skg-get-current-headline-text))))
         (label (skg--replace-headline-links-with-labels title))
         (replacement (format "%s[[id:%s][%s]]\n" stars id label))
         (start (line-beginning-position))
         (end (save-excursion
                (org-end-of-subtree t t))))
    (org-fold-core-ignore-fragility-checks
      (delete-region start end)
      (insert replacement))))

(defun skg--single-link-in-current-leaf ()
  "Return plist data for the only org bracket link in this leaf.
Signals a user error if the current node has org-descendents, if
there is not exactly one link in its title plus body, or if that
one link is not an id link."
  (when (skg--current-node-has-org-descendents-p)
    (user-error "Cannot replace link with content: node has org-descendents"))
  (let* ((headline-text (skg-get-current-headline-text))
         (split (skg-split-as-stars-metadata-title headline-text))
         (title (nth 2 split))
         (body (skg--current-node-body-text))
         (links (skg--links-in-text (concat title "\n" body))))
    (unless (= (length links) 1)
      (user-error "Cannot replace link with content: expected exactly one link, found %d"
                  (length links)))
    (let* ((link (car links))
           (target (plist-get link :target)))
      (unless (string-prefix-p "id:" target)
        (user-error "Cannot replace link with content: the link is not an id link"))
      (plist-put link :id (substring target 3)))))

(defun skg--current-node-has-org-descendents-p ()
  "Return non-nil if the current org node has child headlines."
  (save-excursion
    (let ((level (org-outline-level))
          (subtree-end (save-excursion
                         (org-end-of-subtree t t))))
      (outline-next-heading)
      (and (< (point) subtree-end)
           (> (org-outline-level) level)))))

(defun skg--current-node-body-text ()
  "Return the current node body text, excluding the headline."
  (let ((start (save-excursion
                 (forward-line 1)
                 (point)))
        (end (save-excursion
               (org-end-of-subtree t t))))
    (buffer-substring-no-properties start end)))

(defun skg--links-in-text (text)
  "Return plist data for every org bracket link in TEXT."
  (let ((start 0)
        (links nil))
    (while (string-match skg--headline-org-link-regex text start)
      (push (list :target (match-string 1 text)
                  :label (match-string 2 text))
            links)
      (setq start (match-end 0)))
    (nreverse links)))

(defun skg--replace-current-leaf-with-linked-content (link)
  "Replace the current leaf with an indefinitive TrueNode for LINK."
  (let* ((stars (nth 0 (skg-split-as-stars-metadata-title
                       (skg-get-current-headline-text))))
         (id (plist-get link :id))
         (label (or (plist-get link :label) id))
         (replacement (format "%s(skg (node (id %s) indef (viewRequests definitiveView))) %s\n"
                              stars id label))
         (start (line-beginning-position))
         (end (save-excursion
                (org-end-of-subtree t t))))
    (org-fold-core-ignore-fragility-checks
      (delete-region start end)
      (insert replacement))))

(defun skg--headline-title-has-link-p (title)
  "Return non-nil if TITLE contains an org bracket link."
  (string-match-p skg--headline-org-link-regex title))

(defun skg--replace-headline-links-with-labels (title)
  "Return TITLE with each org bracket link replaced by plain text."
  (replace-regexp-in-string
   skg--headline-org-link-regex
   (lambda (link)
     (if (match-string 2 link)
         (match-string 2 link)
       (match-string 1 link)))
   title))

(provide 'skg-modify-graph)
