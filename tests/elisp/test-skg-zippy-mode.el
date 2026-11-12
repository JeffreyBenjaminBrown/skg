;;; test-skg-zippy-mode.el --- Tests for skg-zippy-mode -*- lexical-binding: t; -*-
;;;
;;; Keys are typed with 'execute-kbd-macro' into a buffer shown in the
;;; selected window, so they go through the real key lookup.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-request-save) ;; defines skg-report-mode
(require 'skg-zippy-mode)

(defvar test-skg-zippy--calls nil
  "The prefix arguments 'test-skg-zippy--command' was called with, latest first.")

(defun test-skg-zippy--command (arg)
  "A command bound under C-c by the tests below."
  (interactive "P")
  (push arg test-skg-zippy--calls))

(defmacro test-skg-zippy--with-buffers (bindings &rest body)
  "Bind each (VAR MAKER) of BINDINGS to a new buffer in which MAKER
has run, run BODY, then leave zippy, kill the buffers, and unbind
C-c ~ from 'skg-content-view-mode-map'."
  (declare (indent 1))
  `(let ((test-skg-zippy--calls nil)
         ,@(mapcar (lambda (binding)
                     `(,(car binding)
                       (with-current-buffer
                           (generate-new-buffer
                            ,(format "*test-zippy-%s*" (car binding)))
                         ,(cadr binding)
                         (current-buffer))))
                   bindings))
     (unwind-protect
         (progn ,@body)
       (skg-zippy-mode -1)
       (define-key skg-content-view-mode-map (kbd "C-c ~") nil t)
       ,@(mapcar (lambda (binding)
                   `(when (buffer-live-p ,(car binding))
                      (with-current-buffer ,(car binding)
                        (set-buffer-modified-p nil))
                      (kill-buffer ,(car binding))))
                 bindings))))

(defun test-skg-zippy--type-into (buffer keys)
  "Type KEYS, a 'kbd' string, into BUFFER as the selected window's buffer."
  (switch-to-buffer buffer)
  (execute-kbd-macro (kbd keys)))

(ert-deftest test-skg-zippy-typed-keys-run-the-C-c-command-and-insert-nothing ()
  "In zippy, a typed key sequence runs what C-c plus it runs, even when
bound under C-c after zippy came on, and a digit gives the command a
prefix argument. An unbound printable key inserts nothing. C-c is
still the ordinary prefix."
  (test-skg-zippy--with-buffers
      ((view (skg-content-view-mode)))
    (with-current-buffer view (insert "* a headline"))
    (switch-to-buffer view)
    (skg-zippy-mode 1)
    (define-key skg-content-view-mode-map (kbd "C-c ~ q")
                #'test-skg-zippy--command)
    (test-skg-zippy--type-into view "~ q")
    (should (equal test-skg-zippy--calls '(nil)))
    (test-skg-zippy--type-into view "4 ~ q")
    (should (equal test-skg-zippy--calls '(4 nil)))
    (test-skg-zippy--type-into view "x y SPC")
    (test-skg-zippy--type-into view "C-c ~ q")
    (should (equal test-skg-zippy--calls '(nil 4 nil)))
    (with-current-buffer view
      (should (equal (buffer-string) "* a headline")))))

(ert-deftest test-skg-zippy-leaves-org-C-c-keys-out ()
  "A content view inherits org's C-c keys, but zippy takes only skg's:
* is unbound in zippy, and C-c C-c is still org's."
  (test-skg-zippy--with-buffers
      ((view (skg-content-view-mode)))
    (switch-to-buffer view)
    (skg-zippy-mode 1)
    (should (eq (key-binding "*") #'skg-zippy-unbound-key))
    (should (eq (key-binding (kbd "C-c C-c")) #'org-ctrl-c-ctrl-c))
    (should (eq (key-binding (kbd "C-o")) #'skg-goto))
    (should (eq (key-binding (kbd "v d")) #'skg-diffView-open))))

(ert-deftest test-skg-zippy-is-one-state-for-every-skg-buffer ()
  "C-c z in one skg buffer turns zippy on in every skg buffer, the
report buffer using its own C-c map, and in a view made later; a
buffer that is not an skg buffer still types. A buffer visiting a
.skg file is an skg buffer. z in another skg buffer
turns it off everywhere, restoring each cursor."
  (test-skg-zippy--with-buffers
      ((view-a (skg-content-view-mode))
       (view-b (progn (skg-content-view-mode)
                      (setq-local cursor-type 'bar)))
       (report (progn (org-mode) (skg-report-mode 1)))
       (skg-file (progn (text-mode) (skg-file-minor-mode 1)))
       (plain (org-mode)))
    (test-skg-zippy--type-into view-a "C-c z")
    (should skg-zippy-mode)
    (dolist (skg-buffer (list view-a view-b report skg-file))
      (with-current-buffer skg-buffer
        (should skg-zippy--emulation-alist)
        (should (eq cursor-type 'hollow))
        ;; 'format-mode-line' yields "" in batch Emacs, so this asks
        ;; the lighter's function directly.
        (should (equal (skg-zippy--lighter-for-this-buffer) " Zippy"))))
    (with-current-buffer view-b
      (should (keymapp (key-binding "s"))))
    (with-current-buffer report ;; C-c s is bound only in content views
      (should (eq (key-binding "s") #'skg-zippy-unbound-key)))
    (test-skg-zippy--type-into plain "x")
    (with-current-buffer plain
      (should (equal (buffer-string) "x"))
      (should-not skg-zippy--emulation-alist)
      (should (equal (skg-zippy--lighter-for-this-buffer) "")))
    (let ((later (generate-new-buffer "*test-zippy-later*")))
      (unwind-protect
          (with-current-buffer later
            (skg-content-view-mode)
            (should skg-zippy--emulation-alist))
        (kill-buffer later)))
    (test-skg-zippy--type-into view-b "z")
    (should-not skg-zippy-mode)
    (with-current-buffer view-a
      (should-not skg-zippy--emulation-alist)
      (should-not (local-variable-p 'cursor-type)))
    (with-current-buffer view-b
      (should (eq cursor-type 'bar)))
    (test-skg-zippy--type-into view-b "x")
    (with-current-buffer view-b
      (should (equal (buffer-string) "x")))))

(provide 'test-skg-zippy-mode)
