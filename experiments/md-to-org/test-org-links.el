;;; test-org-links.el --- Test that all org links resolve correctly -*- lexical-binding: t; -*-

;;; Commentary:
;; This script tests all org links in the docs/ and README.org files.
;; For links with GitHub-style anchors, it verifies the heading exists.
;; For links without anchors, it verifies the file exists.
;; Broken links are reported with their source location.

;;; Code:

(require 'org)

(defvar test-org-links-base-dir "/home/ubuntu/"
  "Base directory for the org files.")

(defun test-org-links--heading-to-anchor (heading)
  "Convert an org HEADING to GitHub-style anchor format."
  (let ((anchor (downcase heading)))
    (setq anchor (replace-regexp-in-string "[^a-z0-9 -]" "" anchor))
    (setq anchor (replace-regexp-in-string " +" "-" anchor))
    (setq anchor (replace-regexp-in-string "^-+\\|-+$" "" anchor))
    anchor))

(defun test-org-links--find-heading-by-anchor (file anchor)
  "Check if FILE contains a heading matching ANCHOR.
Returns (heading-text . line-number) if found, nil otherwise."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found)
                    (re-search-forward org-heading-regexp nil t))
          (let* ((heading-text (org-get-heading t t t t))
                 (heading-anchor (test-org-links--heading-to-anchor heading-text))
                 (line-num (line-number-at-pos)))
            (when (string= heading-anchor anchor)
              (setq found (cons heading-text line-num)))))
        found))))

(defun test-org-links--list-headings (file)
  "List all headings in FILE with their anchor equivalents."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (let ((headings nil))
        (while (re-search-forward org-heading-regexp nil t)
          (let* ((heading-text (org-get-heading t t t t))
                 (heading-anchor (test-org-links--heading-to-anchor heading-text)))
            (push (cons heading-anchor heading-text) headings)))
        (nreverse headings)))))

(defun test-org-links--extract-links (file)
  "Extract all org links from FILE.
Returns a list of (link-path . link-text) pairs."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((links nil))
      (while (re-search-forward org-link-bracket-re nil t)
        (let ((link (match-string 1))
              (text (or (match-string 2) "")))
          (push (cons link text) links)))
      (nreverse links))))

(defun test-org-links--resolve-path (link source-file)
  "Resolve LINK path relative to SOURCE-FILE."
  (let ((source-dir (file-name-directory source-file)))
    (cond
     ;; External link
     ((string-match "^https?://" link)
      nil)
     ;; Absolute path
     ((string-prefix-p "/" link)
      link)
     ;; Relative path
     (t
      (expand-file-name link source-dir)))))

(defun test-org-links--test-single-link (link text source-file)
  "Test a single LINK with TEXT from SOURCE-FILE.
Returns nil if OK, or an error description string."
  ;; Skip external links
  (when (not (string-match "^https?://" link))
    (let* ((has-anchor (string-match "\\(.+\\)#\\(.+\\)$" link))
           (file-part (if has-anchor (match-string 1 link) link))
           (anchor (when has-anchor (match-string 2 link)))
           (resolved-path (test-org-links--resolve-path file-part source-file))
           (heading-result (when (and anchor (file-exists-p resolved-path))
                             (test-org-links--find-heading-by-anchor resolved-path anchor))))
      (cond
       ;; File doesn't exist
       ((not (file-exists-p resolved-path))
        (format "File not found: %s" resolved-path))
       ;; Has anchor but heading not found
       ((and anchor (not heading-result))
        (format "Anchor '#%s' not found in %s\n  Available headings:\n%s"
                anchor
                (file-name-nondirectory resolved-path)
                (mapconcat
                 (lambda (h) (format "    #%s -> \"%s\"" (car h) (cdr h)))
                 (test-org-links--list-headings resolved-path)
                 "\n")))
       ;; Has anchor but heading is on line 1 (would look like going to top of file)
       ((and anchor heading-result (= (cdr heading-result) 1))
        (format "Anchor '#%s' resolves to line 1 in %s (heading: \"%s\")\n  This is indistinguishable from landing at top of file"
                anchor
                (file-name-nondirectory resolved-path)
                (car heading-result)))
       ;; OK
       (t nil)))))

(defun test-org-links--test-file (file)
  "Test all links in FILE. Returns list of error messages."
  (let ((links (test-org-links--extract-links file))
        (errors nil))
    (dolist (link-pair links)
      (let* ((link (car link-pair))
             (text (cdr link-pair))
             (error-msg (test-org-links--test-single-link link text file)))
        (when error-msg
          (push (format "In %s:\n  Link: [[%s][%s]]\n  Error: %s"
                        (file-relative-name file test-org-links-base-dir)
                        link text error-msg)
                errors))))
    (nreverse errors)))

(defun test-org-links-run ()
  "Run link tests on all org files in docs/ and README.org."
  (interactive)
  (let ((files (append
                (list (expand-file-name "README.org" test-org-links-base-dir))
                (directory-files-recursively
                 (expand-file-name "docs" test-org-links-base-dir)
                 "\\.org$")))
        (all-errors nil)
        (total-links 0)
        (broken-links 0))
    (dolist (file files)
      (let ((links (test-org-links--extract-links file))
            (errors (test-org-links--test-file file)))
        (setq total-links (+ total-links (length links)))
        (setq broken-links (+ broken-links (length errors)))
        (setq all-errors (append all-errors errors))))

    ;; Output results
    (princ (format "=== Org Link Test Results ===\n\n"))
    (princ (format "Files tested: %d\n" (length files)))
    (princ (format "Total links: %d\n" total-links))
    (princ (format "Broken links: %d\n\n" broken-links))

    (if all-errors
        (progn
          (princ "=== Broken Links ===\n\n")
          (dolist (err all-errors)
            (princ err)
            (princ "\n\n")))
      (princ "All links OK!\n"))

    (princ "=== End of Report ===\n")))

;; Run when loaded in batch mode
(when noninteractive
  (test-org-links-run))

(provide 'test-org-links)

;;; test-org-links.el ends here
