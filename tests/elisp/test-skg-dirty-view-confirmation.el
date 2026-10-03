;;; test-skg-dirty-view-confirmation.el --- Tests for confirming before dirtying a second view

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-request-save)

(defmacro skg-test--with-views (names &rest body)
  "Bind each of NAMES to a fresh, clean view buffer around BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (name)
                   `(,name (skg-open-org-buffer-from-text
                            nil "* (skg (node (id x))) x\n"
                            ,(format "*test-dirty-%s*" name))))
                 names)
     (unwind-protect (progn ,@body)
       (dolist (buf (list ,@names))
         (when (buffer-live-p buf) (kill-buffer buf))))))

(defun skg-test--edit-answering (buffer answer)
  "Insert into BUFFER, answering ANSWER to any confirmation.
Return the number of times confirmation was asked."
  (let (( asked 0 ))
    (cl-letf (( (symbol-function 'yes-or-no-p)
                (lambda (_prompt) (cl-incf asked) answer) ))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "edit\n")))
    asked))

(ert-deftest test-skg-dirty-view-confirmation-alone-does-not-ask ()
  "Dirtying the only dirty view asks nothing."
  (skg-test--with-views (a b)
    (should (= 0 (skg-test--edit-answering a nil)))
    (should (buffer-modified-p a))))

(ert-deftest test-skg-dirty-view-confirmation-declined-cancels-edit ()
  "Declining leaves the second view unchanged and clean."
  (skg-test--with-views (a b)
    (skg-test--edit-answering a nil)
    (let (( before (with-current-buffer b (buffer-string)) ))
      (should-error (skg-test--edit-answering b nil) :type 'user-error)
      (with-current-buffer b
        (should (equal (buffer-string) before))
        (should-not (buffer-modified-p))))))

(ert-deftest test-skg-dirty-view-confirmation-asks-for-each-new-view ()
  "Accepting lets the edit through; a third view asks again, once."
  (skg-test--with-views (a b c)
    (skg-test--edit-answering a nil)
    (should (= 1 (skg-test--edit-answering b t)))
    (should (buffer-modified-p b))
    (should (= 0 (skg-test--edit-answering b t))) ; already dirty
    (should (= 1 (skg-test--edit-answering c t)))))

(ert-deftest test-skg-dirty-view-confirmation-ignores-uri-less-buffers ()
  "A content-view-mode buffer with no view URI (like the fork
confirmation buffer) neither asks nor counts as dirty."
  (skg-test--with-views (a b)
    (with-current-buffer b (setq skg-view-uri nil))
    (skg-test--edit-answering a nil)
    (should (= 0 (skg-test--edit-answering b nil)))
    (with-current-buffer a (set-buffer-modified-p nil))
    (should (= 0 (skg-test--edit-answering a nil)))))

(ert-deftest test-skg-dirty-view-confirmation-quiet-during-rerender ()
  "Skg's own replacement of view text does not ask."
  (skg-test--with-views (a b)
    (skg-test--edit-answering a nil)
    (cl-letf (( (symbol-function 'yes-or-no-p)
                (lambda (_prompt) (error "asked during rerender")) ))
      (with-current-buffer b
        (skg-replace-buffer-with-new-content nil "* (skg (node (id y))) y\n")
        (should-not (buffer-modified-p))))))

;;; test-skg-dirty-view-confirmation.el ends here
