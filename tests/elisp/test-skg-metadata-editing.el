;;; test-skg-metadata-editing.el --- Tests for skg metadata editing functions

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-metadata)

(defconst example-data
  "* 1
1 body
* (skg) 2
* (skg (k v) value) 3
3 body
* (skg value (k v)) 4"
  "Immutable example data for metadata editing tests.")

(defun apply-to-all-lines
    (func &rest args)
  "Apply FUNC with ARGS to every line in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (apply func args)
      (forward-line 1))))

(ert-deftest test-delete-kv-pair-key-exists ()
  "Test deleting a kv-pair when the key exists."
  (let ((buf (generate-new-buffer
              "*test-delete-kv-exists*")))
    (with-current-buffer buf
      (org-mode)
      (insert example-data)
      (apply-to-all-lines
       #'skg-delete-kv-pair-from-metadata-by-key "k")
      (should (equal (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     "* 1
1 body
* (skg) 2
* (skg value) 3
3 body
* (skg value) 4")))
    (kill-buffer buf)))

(ert-deftest test-delete-kv-pair-key-not-exists ()
  "Test deleting a kv-pair when the key does not exist."
  (let ((buf (generate-new-buffer
              "*test-delete-kv-not-exists*")))
    (with-current-buffer buf
      (org-mode)
      (insert example-data)
      (apply-to-all-lines
       #'skg-delete-kv-pair-from-metadata-by-key "h")
      (should (equal (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     "* 1
1 body
* (skg) 2
* (skg (k v) value) 3
3 body
* (skg (k v) value) 4")))
    (kill-buffer buf)))

(ert-deftest test-delete-value-exists ()
  "Test deleting a value when it exists."
  (let ((buf (generate-new-buffer
              "*test-delete-value-exists*")))
    (with-current-buffer buf
      (org-mode)
      (insert example-data)
      (apply-to-all-lines
       #'skg-delete-value-from-metadata "value")
      (should (equal (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     "* 1
1 body
* (skg) 2
* (skg (k v)) 3
3 body
* (skg (k v)) 4")))
    (kill-buffer buf)))

(ert-deftest test-delete-value-not-exists ()
  "Test deleting a value when it does not exist."
  (let ((buf (generate-new-buffer
              "*test-delete-value-not-exists*")))
    (with-current-buffer buf
      (org-mode)
      (insert example-data)
      (apply-to-all-lines
       #'skg-delete-value-from-metadata "nonexistent")
      (should (equal (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     "* 1
1 body
* (skg) 2
* (skg (k v) value) 3
3 body
* (skg (k v) value) 4")))
    (kill-buffer buf)))

(require 'skg-sexpr-edit)

(ert-deftest test-sexp-edit-herald-hints-appear-as-overlays ()
  "The metadata view appends, per field/value headline, the herald it
produces -- as an overlay (never buffer text), colored tokens inside
plain parens (TODO/more.org). Fields producing no herald get none."
  (skg-test-install-herald-rules)
  (with-temp-buffer
    (insert "* skg\n** node\n*** source\n**** main\n*** indef\n"
            "*** parentIs\n**** independent\n")
    (org-mode)
    (skg-sexp-edit--decorate-with-heralds)
    (let ((hint-on-line
           (lambda (needle)
             (goto-char (point-min))
             (search-forward needle)
             (let ((ov (seq-find
                        (lambda (o)
                          (overlay-get o 'skg-sexp-edit-herald))
                        ;; 1+: an EMPTY overlay sitting exactly at
                        ;; line-end is excluded by overlays-in when
                        ;; the range stops there.
                        (overlays-in (line-beginning-position)
                                     (min (point-max)
                                          (1+ (line-end-position)))))))
               (and ov (substring-no-properties
                        (overlay-get ov 'after-string)))))))
      (should (equal (funcall hint-on-line "*** indef") " (\u262e)"))
      (should (equal (funcall hint-on-line "**** independent") " (\u22a5)"))
      (should-not (funcall hint-on-line "**** main"))
      (should-not (funcall hint-on-line "*** source"))
      ;; The buffer TEXT is untouched: committing must not see hints.
      (should-not (string-match-p "(\u262e)" (buffer-string))))))

(provide 'test-skg-metadata-editing)
