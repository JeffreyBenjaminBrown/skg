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

;; The confirmation is asked from a timer, after the editing command
;; has finished.  These helpers capture that timer's call and run it.

(defun skg-test--edit-capturing-question (buffer edit-function)
  "Call EDIT-FUNCTION in BUFFER, failing if it prompts mid-edit.
Return the deferred question as (FUNCTION . ARGS), or nil if none."
  (let (( deferred nil ))
    (cl-letf (( (symbol-function 'run-at-time)
                (lambda (_time _repeat function &rest args)
                  (setq deferred (cons function args))) )
              ( (symbol-function 'yes-or-no-p)
                (lambda (_prompt) (error "Prompted in the middle of an edit")) ))
      (with-current-buffer buffer
        (goto-char (point-max))
        (condition-case nil (funcall edit-function)
          (user-error nil))))
    deferred))

(defun skg-test--edit-answering (buffer answer)
  "Edit BUFFER; if that asks, answer ANSWER and, on yes, repeat the edit.
Return the number of times confirmation was asked."
  (let* (( edit (lambda () (insert "edit\n")) )
         ( deferred (skg-test--edit-capturing-question buffer edit) ))
    (if (null deferred)
        0
      (cl-letf (( (symbol-function 'yes-or-no-p) (lambda (_prompt) answer) ))
        (apply (car deferred) (cdr deferred)))
      (when answer
        (with-current-buffer buffer (funcall edit)))
      1)))

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
      (should (= 1 (skg-test--edit-answering b nil)))
      (with-current-buffer b
        (should (equal (buffer-string) before))
        (should-not (buffer-modified-p))))))

(ert-deftest test-skg-dirty-view-confirmation-asks-for-each-new-view ()
  "Approving lets the repeated edit through; a third view asks again, once."
  (skg-test--with-views (a b c)
    (skg-test--edit-answering a nil)
    (should (= 1 (skg-test--edit-answering b t)))
    (should (buffer-modified-p b))
    (should (= 0 (skg-test--edit-answering b t))) ; already dirty
    (should (= 1 (skg-test--edit-answering c t)))
    (should (buffer-modified-p c))))

(ert-deftest test-skg-dirty-view-confirmation-asks-after-newline-finishes ()
  "A first edit made by `newline' does not prompt inside `newline'.
Prompting there let newline's temporary `post-self-insert-hook' move
point back after each typed character, reversing the answer."
  (skg-test--with-views (a b)
    (skg-test--edit-answering a nil)
    (should (skg-test--edit-capturing-question b #'newline))))

(ert-deftest test-skg-dirty-view-confirmation-replays-the-command-keys ()
  "Approving queues the cancelled command's keys to run again."
  (skg-test--with-views (a b)
    (skg-test--edit-answering a nil)
    (switch-to-buffer b)
    (let* (( unread-command-events nil )
           ( deferred
             (let (( this-command 'self-insert-command ))
               (cl-letf (( (symbol-function 'this-command-keys-vector)
                           (lambda () [?x]) ))
                 (skg-test--edit-capturing-question b (lambda () (insert "x")))) )) )
      (cl-letf (( (symbol-function 'yes-or-no-p) (lambda (_prompt) t) ))
        (apply (car deferred) (cdr deferred)))
      (should (equal unread-command-events '(?x)))
      (should (eq skg--dirtying-view-approved b))
      (skg--forget-dirtying-view-approval))))

(ert-deftest test-skg-dirty-view-confirmation-approval-ends-when-point-leaves ()
  "Approval survives commands in the approved view, and is revoked by
the first command after which point is in another buffer, even though
the approved view stays visible."
  (skg-test--with-views (a b)
    (skg-test--edit-answering a nil)
    (switch-to-buffer b)
    (let (( deferred (skg-test--edit-capturing-question
                      b (lambda () (insert "x"))) ))
      (cl-letf (( (symbol-function 'yes-or-no-p) (lambda (_prompt) t) ))
        (apply (car deferred) (cdr deferred))))
    (unwind-protect
        (progn
          (run-hooks 'post-command-hook)
          (should (eq skg--dirtying-view-approved b))
          (delete-other-windows)
          (split-window)
          (switch-to-buffer a) ; b remains visible in the other window
          (run-hooks 'post-command-hook)
          (should-not skg--dirtying-view-approved)
          (should-not (memq #'skg--revoke-dirtying-view-approval-if-point-left
                            (default-value 'post-command-hook)))
          (switch-to-buffer b)
          (should (skg-test--edit-capturing-question
                   b (lambda () (insert "x")))))
      (skg--forget-dirtying-view-approval)
      (delete-other-windows))))

(ert-deftest test-skg-dirty-view-confirmation-ignores-view-id-less-buffers ()
  "A content-view-mode buffer with no view ID (like the fork
confirmation buffer) neither asks nor counts as dirty."
  (skg-test--with-views (a b)
    (with-current-buffer b (setq skg-view-id nil))
    (skg-test--edit-answering a nil)
    (should (= 0 (skg-test--edit-answering b nil)))
    (with-current-buffer a (set-buffer-modified-p nil))
    (should (= 0 (skg-test--edit-answering a nil)))))

(ert-deftest test-skg-dirty-view-confirmation-quiet-during-rerender ()
  "Skg's own replacement of view text does not ask."
  (skg-test--with-views (a b)
    (skg-test--edit-answering a nil)
    (cl-letf (( (symbol-function 'run-at-time)
                (lambda (&rest _) (error "asked during rerender")) ))
      (with-current-buffer b
        (skg-replace-buffer-with-new-content nil "* (skg (node (id y))) y\n")
        (should-not (buffer-modified-p))))))

;;; test-skg-dirty-view-confirmation.el ends here
