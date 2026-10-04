;;; -*- lexical-binding: t; -*-
;;; Tests for display-only Skg link annotations.

(require 'ert)
(require 'skg-buffer)
(require 'skg-link-annotations)

(ert-deftest test-skg-link-annotations-style-and-toggle-without-text-edits ()
  (let ((skg-link-annotations--cache (make-hash-table :test 'equal)))
    (puthash "old-visible" '(resolved "visible" "pub")
             skg-link-annotations--cache)
    (puthash "gone" '(missing) skg-link-annotations--cache)
    (puthash "private" '(inactive) skg-link-annotations--cache)
    (with-temp-buffer
      (insert "* Unicode [[id:old-visible][café]] and [[id:gone][gone]]\n"
              "Body [[id:private][private]] and [[https://x][web]]\n")
      (skg-content-view-mode)
      (set-buffer-modified-p nil)
      (let ((original (buffer-string)))
        (skg-link-annotations-mode 1)
        (let* ((broken (cl-find-if
                        (lambda (overlay)
                          (overlay-get overlay 'face))
                        (overlays-in (point-min) (point-max))))
               (face (and broken (overlay-get broken 'face))))
          (should broken)
          (should (eq (plist-get face :inherit)
                      'heralds-yucky-face))
          (should (plist-get face :underline)))
        (should-not
         (cl-some (lambda (overlay) (overlay-get overlay 'after-string))
                  (overlays-in (point-min) (point-max))))
        (skg-toggle-repo-overlay-on-links)
        (let ((suffixes (cl-loop for overlay in
                                 (overlays-in (point-min) (point-max))
                                 for text = (overlay-get overlay 'after-string)
                                 when text collect (substring-no-properties text))))
          (should (member " [⌂:pub]" suffixes))
          (should (member " [⌂:missing]" suffixes))
          (should (member " [⌂:inactive]" suffixes))
          (should (= (length suffixes) 3)))
        (should (equal (buffer-string) original))
        (should-not (buffer-modified-p))
        (skg-content-view-mode)
        (should skg-link-annotations--repo-suffix-enabled)
        (skg-link-annotations-mode 1)
        (skg-toggle-repo-overlay-on-links)
        (should-not skg-link-annotations--repo-suffix-enabled)
        (should (equal (buffer-string) original))))))

(ert-deftest test-skg-link-annotations-discard-stale-and-mismatched-replies ()
  (let ((skg-link-annotations--cache (make-hash-table :test 'equal))
        (skg-link-annotations--requests (make-hash-table :test 'equal))
        (skg-link-annotations--epoch 20)
        (first (generate-new-buffer " *skg-link-first*"))
        (second (generate-new-buffer " *skg-link-second*")))
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (insert "* [[id:node][label]]\n")
              (skg-content-view-mode)
              (skg-link-annotations-mode 1)))
          (with-current-buffer first
            (puthash "old" (list first skg-link-annotations--generation
                                 (buffer-chars-modified-tick) 20 '("node"))
                     skg-link-annotations--requests)
            (insert "edited"))
          (with-current-buffer second
            (puthash "right" (list second skg-link-annotations--generation
                                   (buffer-chars-modified-tick) 20 '("node"))
                     skg-link-annotations--requests))
          (clrhash skg-link-annotations--cache)
          (skg-link-annotations--handle-response
           nil "((request-id \"old\") (results ((\"node\" missing))))")
          (should-not (gethash "node" skg-link-annotations--cache))
          (skg-link-annotations--handle-response
           nil "((request-id \"right\") (results ((\"node\" resolved \"node\" \"main\"))))")
          (should (equal (gethash "node" skg-link-annotations--cache)
                         '(resolved "node" "main")))
          (with-current-buffer second
            (let ((skg-link-annotations--repo-suffix-enabled t))
              (skg-link-annotations-refresh)
              (should (cl-some
                       (lambda (overlay)
                         (equal (substring-no-properties
                                 (or (overlay-get overlay 'after-string) ""))
                                " [⌂:main]"))
                       (overlays-in (point-min) (point-max))))))
          (with-current-buffer second
            (puthash "old-repo-set"
                     (list second skg-link-annotations--generation
                           (buffer-chars-modified-tick) 20 '("node"))
                     skg-link-annotations--requests))
          (setq skg-link-annotations--epoch 21)
          (skg-link-annotations--handle-response
           nil "((request-id \"old-repo-set\") (results ((\"node\" missing))))")
          (should (equal (gethash "node" skg-link-annotations--cache)
                         '(resolved "node" "main")))
          (puthash "dead-buffer" (list first 1 1 21 '("node"))
                   skg-link-annotations--requests)
          (kill-buffer first)
          (skg-link-annotations--handle-response
           nil "((request-id \"dead-buffer\") (results ((\"node\" missing))))")
          (should (equal (gethash "node" skg-link-annotations--cache)
                         '(resolved "node" "main")))
          (puthash "old-connection" (list second 1 1 21 '("node"))
                   skg-link-annotations--requests)
          (skg-link-annotations-connection-reset)
          (should-not (gethash "old-connection"
                               skg-link-annotations--requests)))
      (kill-buffer first)
      (kill-buffer second))))

;; (NAME TEXT LIVE-IDS) for each case in the file, which the Rust and
;; Neovim tests read too.
(defconst test-skg-link-annotations--shared-cases-file
  (expand-file-name "../shared/literal-link-cases.txt"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun test-skg-link-annotations--shared-cases ()
  "Parse `test-skg-link-annotations--shared-cases-file'."
  (let ((cases nil) (name nil) (text nil))
    (dolist (line (split-string
                   (with-temp-buffer
                     (insert-file-contents test-skg-link-annotations--shared-cases-file)
                     (buffer-string))
                   "\n"))
      (cond ((string-prefix-p "==== " line)
             (setq name (substring line 5) text nil))
            ((string-prefix-p "---- live:" line)
             (push (list name (string-join (nreverse text) "\n")
                         (split-string (substring line 10)))
                   cases)
             (setq name nil))
            (name (push line text))))
    (nreverse cases)))

(ert-deftest test-skg-link-annotations-shared-literal-link-cases ()
  "Only real links are annotated, per the cases shared with Rust and Neovim."
  (let ((cases (test-skg-link-annotations--shared-cases)))
    (should (> (length cases) 5))
    (dolist (case cases)
      (with-temp-buffer
        (insert (nth 1 case))
        (should (equal (list (car case)
                             (mapcar (lambda (position) (nth 3 position))
                                     (skg-link-annotations--scan)))
                       (list (car case) (nth 2 case))))))))

(ert-deftest test-skg-link-annotations-headline-ends-a-block ()
  "A client sees whole buffers, so a headline ends an unclosed block,
as the end of a node's body does on the server."
  (with-temp-buffer
    (insert "* h\n#+begin_example\n[[id:a][A]]\n* next [[id:b][B]]\n")
    (should (equal (mapcar (lambda (position) (nth 3 position))
                           (skg-link-annotations--scan))
                   '("b")))))
