;;; test-skg-metadata-editing.el --- Tests for skg metadata editing functions

(add-to-list
 'load-path
 (expand-file-name
  "../../elisp"
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

(provide 'test-skg-metadata-editing)
