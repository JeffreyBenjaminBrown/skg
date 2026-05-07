;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: User-facing commands that modify graph structure.

(require 'org)
(require 'org-fold-core)
(require 'skg-config)
(require 'skg-metadata)

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

(defconst skg--headline-org-link-regex
  "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
  "Regex matching an org bracket link in a headline title.")

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
