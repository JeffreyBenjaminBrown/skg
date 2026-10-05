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

(defun skg-goto-biggest-branch ()
  "Go to the sibling, or child, with the most viewdescendants.
If the node at point has siblings, consider the node itself and
all of those siblings.  If it has no siblings, consider its
immediate viewchildren instead.  This is intentionally an org
outline heuristic, not a precise graph-content query."
  (interactive)
  (let* ((candidates (skg--biggest-branch-candidates))
         (winner (skg--biggest-branch-max-by-descendents candidates))
         (pos (car winner))
         (descendent-count (cdr winner)))
    (goto-char pos)
    (message "Biggest branch has %d viewdescendant(s)."
             descendent-count)))

(defun skg--biggest-branch-candidates ()
  "Return candidate headline positions for `skg-goto-biggest-branch'."
  (save-excursion
    (skg--back-to-current-headline)
    (let ((same-level-headlines
           (skg--same-level-headlines-in-parent-subtree)))
      (if (> (length same-level-headlines) 1)
          same-level-headlines
        (let ((children (skg--immediate-viewchildren)))
          (unless children
            (user-error "This branch has no siblings or children"))
          children)))))

(defun skg--biggest-branch-max-by-descendents (positions)
  "Return (POSITION . DESCENDENT-COUNT) with greatest count in POSITIONS."
  (let ((best-pos nil)
        (best-count -1))
    (dolist (pos positions)
      (let ((count (skg--viewdescendant-count pos)))
        (when (> count best-count)
          (setq best-pos pos
                best-count count))))
    (cons best-pos best-count)))

(defun skg--same-level-headlines-in-parent-subtree ()
  "Return headline positions at the current headline's org level."
  (let* ((level (org-current-level))
         (bounds (skg--parent-subtree-or-buffer-bounds)))
    (skg--headline-positions-with-level level
                                       (car bounds)
                                       (cdr bounds))))

(defun skg--parent-subtree-or-buffer-bounds ()
  "Return bounds for current headline's parent subtree, or whole buffer."
  (save-excursion
    (if (org-up-heading-safe)
        (cons (line-beginning-position)
              (save-excursion (org-end-of-subtree t t)))
      (cons (point-min) (point-max)))))

(defun skg--immediate-viewchildren ()
  "Return positions of the current headline's immediate viewchildren."
  (let* ((child-level (1+ (org-current-level)))
         (start (line-beginning-position))
         (end (save-excursion (org-end-of-subtree t t))))
    (skg--headline-positions-with-level child-level start end)))

(defun skg--headline-positions-with-level (level start end)
  "Return headline positions of org LEVEL between START and END."
  (let ((positions nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward org-heading-regexp end t)
        (beginning-of-line)
        (when (= (org-current-level) level)
          (push (point) positions))
        (forward-line 1)))
    (nreverse positions)))

(defun skg--viewdescendant-count (pos)
  "Return number of viewdescendant headlines under POS."
  (save-excursion
    (goto-char pos)
    (let ((start (line-beginning-position))
          (end (save-excursion (org-end-of-subtree t t)))
          (count 0))
      (goto-char start)
      (while (re-search-forward org-heading-regexp end t)
        (beginning-of-line)
        (unless (= (point) start)
          (setq count (1+ count)))
        (forward-line 1))
      count)))

(defun skg--back-to-current-headline ()
  "Move to the headline containing point, or signal a user error."
  (condition-case nil
      (org-back-to-heading t)
    (error (user-error "Not in an org headline or body"))))

(defun skg-replace-content-with-link ()
  "Replace the branch at point with a link to its former root.
Point may be on the headline or in its body.  The root must be an
existing UnrestrictedVognode with an ID.  Its viewparent must be a editable
UnrestrictedVognode whose repo is owned by the user.  The whole org subtree
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
viewdescendants, and a editable UnrestrictedVognode viewparent whose repo
is owned by the user.  The link must be an id link.  The leaf is
replaced by a write-protected same-level UnrestrictedVognode for the link
target, then the buffer is saved."
  (interactive)
  (org-back-to-heading t)
  (let* ((parent (skg--content-link-replacement-container-data))
         (node-skgid (skg--node-id
                   (skg--metadata-sexp-at-point-or-nil)))
         (link (skg--single-link-in-current-leaf)))
    (skg--check-content-link-replacement-container parent)
    (when node-skgid
      (message "Warning: replacing existing node %s may have created an orphan"
               node-skgid))
    (skg--replace-current-leaf-with-linked-content link)
    (skg-request-save-buffer)))

(defun skg--content-link-replacement-node-data ()
  "Return plist data for the UnrestrictedVognode headline at point."
  (let* ((headline-text (skg-get-current-headline-text))
         (split (skg-split-as-stars-metadata-title headline-text))
         (metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
    (unless (skg--unrestrictedNode-sexp-p metadata-sexp)
      (user-error "Cannot replace this branch with a link: it is not an unrestrictedNode"))
    (let ((skgid (skg--node-id metadata-sexp)))
      (unless skgid
        (user-error "Cannot replace this branch with a link: node has no ID"))
      (list :id skgid
            :title (nth 2 split)))))

(defun skg--content-link-replacement-container-data ()
  "Return metadata for the viewparent of the headline at point."
  (save-excursion
    (unless (org-up-heading-safe)
      (user-error "Cannot replace this branch with a link: node has no container"))
    (let ((metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
      (unless (skg--unrestrictedNode-sexp-p metadata-sexp)
        (user-error "Cannot replace this branch with a link: container is not an unrestrictedNode"))
      metadata-sexp)))

(defun skg--check-content-link-replacement-container (metadata-sexp)
  "Signal a user error if METADATA-SEXP is not an editable container."
  (let ((skgrepo (skg--node-repo metadata-sexp)))
    (unless skgrepo
      (user-error "Cannot replace this branch with a link: container has no repo"))
    (unless (member skgrepo (skg--owned-repos))
      (user-error "Cannot replace this branch with a link: container repo is not owned: %s"
                  skgrepo))
    (when (skg--node-write-protected-p metadata-sexp)
      (user-error "Cannot replace this branch with a link: container is write-protected"))))

(defun skg--replace-current-subtree-with-link-headline (skgid title)
  "Replace the current org subtree with a headline linking to ID.
The default link label is TITLE with any nested links reduced to plain text."
  (let* ((stars (nth 0 (skg-split-as-stars-metadata-title
                       (skg-get-current-headline-text))))
         (default-label (skg--replace-headline-links-with-labels title))
         (label (skg--read-link-label default-label))
         (replacement (format "%s[[id:%s][%s]]\n" stars skgid label))
         (start (line-beginning-position))
         (end (save-excursion
                (org-end-of-subtree t t))))
    (org-fold-core-ignore-fragility-checks
      (delete-region start end)
      (insert replacement))))

(defun skg--single-link-in-current-leaf ()
  "Return plist data for the only org bracket link in this leaf.
Signals a user error if the current node has viewdescendants, if
there is not exactly one link in its title plus body, or if that
one link is not an id link."
  (when (skg--current-node-has-viewdescendants-p)
    (user-error "Cannot replace link with content: node has viewdescendants"))
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

(defun skg--current-node-has-viewdescendants-p ()
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
  "Replace the current leaf with a write-protected UnrestrictedVognode for LINK."
  (let* ((stars (nth 0 (skg-split-as-stars-metadata-title
                       (skg-get-current-headline-text))))
         (skgid (plist-get link :id))
         (label (or (plist-get link :label) skgid))
         (replacement (format "%s(skg (node (id %s) writeProtected (viewRequests editableView))) %s\n"
                              stars skgid label))
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

(defun skg--read-link-label (default-label)
  "Prompt for a link label, defaulting to DEFAULT-LABEL."
  (if (or noninteractive (minibufferp))
      default-label
    (read-string "Link label: " default-label)))

(provide 'skg-modify-graph)
