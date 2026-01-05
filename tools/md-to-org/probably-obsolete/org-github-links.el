;;; org-github-links.el --- Follow GitHub-style anchor links in org files -*- lexical-binding: t; -*-

;; This package adds support for following GitHub-flavored anchor links
;; in org-mode. GitHub converts headings to anchors by:
;; - Converting to lowercase
;; - Replacing spaces with hyphens
;; - Removing most punctuation
;;
;; Example: "## Learn from how others use your ideas."
;; becomes: #learn-from-how-others-use-your-ideas

;;; Code:

(defun org-github-links--heading-to-anchor (heading)
  "Convert an org HEADING to GitHub-style anchor format.
Lowercase, replace spaces with hyphens, remove punctuation."
  (let ((anchor (downcase heading)))
    ;; Remove common punctuation (keep alphanumeric, spaces, hyphens)
    (setq anchor (replace-regexp-in-string "[^a-z0-9 -]" "" anchor))
    ;; Replace spaces with hyphens
    (setq anchor (replace-regexp-in-string " +" "-" anchor))
    ;; Remove leading/trailing hyphens
    (setq anchor (replace-regexp-in-string "^-+\\|-+$" "" anchor))
    anchor))

(defun org-github-links--find-heading-by-anchor (anchor)
  "Find the position of a heading matching GitHub-style ANCHOR.
Returns the position of the heading, or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found)
                  (re-search-forward org-heading-regexp nil t))
        (let* ((heading-text (org-get-heading t t t t))
               (heading-anchor (org-github-links--heading-to-anchor heading-text)))
          (when (string= heading-anchor anchor)
            (setq found (line-beginning-position)))))
      found)))

(defun org-github-links--get-link-at-point ()
  "Get the link path at point, or nil if not on a link."
  (let ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (org-element-property :raw-link context))))

(defun org-github-links-follow-at-point ()
  "Follow the org link at point, handling GitHub-style anchors.
If point is not on a link, display a message and do nothing."
  (interactive)
  (let ((path (org-github-links--get-link-at-point)))
    (if (not path)
        (message "No link at point")
      (org-github-links--follow-path path))))

(defun org-github-links--follow-path (path)
  "Follow an org link PATH that may contain a GitHub-style anchor.
PATH can be like './file.org#anchor-text' or just './file.org'."
  (let* ((parts (split-string path "#"))
         (file-path (car parts))
         (anchor (cadr parts)))
    ;; Open the file
    (org-open-file file-path)
    ;; If there's an anchor, find and jump to it
    (when anchor
      (let ((pos (org-github-links--find-heading-by-anchor anchor)))
        (if pos
            (progn
              (goto-char pos)
              (org-show-entry)
              (recenter 0))
          (message "Warning: anchor '#%s' not found in %s"
                   anchor file-path))))))

(defvar org-github-links-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g") #'org-github-links-follow-at-point)
    map)
  "Keymap for `org-github-links-mode'.")

(defun org-github-links--file-link-handler (path _desc _format)
  "Handle file links that might contain GitHub-style anchors.
Intercepts file: links and handles # anchors specially."
  ;; Check if this is a file link with an anchor
  (if (string-match "#" path)
      (org-github-links--follow-path path)
    ;; No anchor, use default behavior
    nil))

(defun org-github-links-setup ()
  "Set up advice to handle GitHub-style anchors in org links."
  (advice-add 'org-link-open-as-file :before-until
              #'org-github-links--open-file-with-anchor))

(defun org-github-links--open-file-with-anchor (path &optional _arg)
  "Advice function to intercept file links with anchors.
PATH is the link path. Returns non-nil if we handled the link."
  (when (and (stringp path)
             (string-match "\\(.+\\.org\\)#\\(.+\\)$" path))
    (let ((file (match-string 1 path))
          (anchor (match-string 2 path)))
      (find-file file)
      (let ((pos (org-github-links--find-heading-by-anchor anchor)))
        (if pos
            (progn
              (goto-char pos)
              (org-show-entry)
              (recenter 0))
          (message "Warning: anchor '#%s' not found" anchor)))
      t)))  ; Return t to indicate we handled it

;;;###autoload
(define-minor-mode org-github-links-mode
  "Minor mode to enable GitHub-style anchor links in org-mode.
\\{org-github-links-mode-map}"
  :global t
  :lighter " GH-Links"
  :keymap org-github-links-mode-map
  (if org-github-links-mode
      (advice-add 'org-link-open-as-file :before-until
                  #'org-github-links--open-file-with-anchor)
    (advice-remove 'org-link-open-as-file
                   #'org-github-links--open-file-with-anchor)))

;; Automatically enable in org-mode buffers
(add-hook 'org-mode-hook #'org-github-links-mode)

(provide 'org-github-links)

;;; org-github-links.el ends here
