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
turns it off everywhere."
  (test-skg-zippy--with-buffers
      ((view-a (skg-content-view-mode))
       (view-b (skg-content-view-mode))
       (report (progn (org-mode) (skg-report-mode 1)))
       (skg-file (progn (text-mode) (skg-file-minor-mode 1)))
       (plain (org-mode)))
    (test-skg-zippy--type-into view-a "C-c z")
    (should skg-zippy-mode)
    (dolist (skg-buffer (list view-a view-b report skg-file))
      (with-current-buffer skg-buffer
        (should skg-zippy--emulation-alist)
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
      (should-not skg-zippy--emulation-alist))
    (test-skg-zippy--type-into view-b "x")
    (with-current-buffer view-b
      (should (equal (buffer-string) "x")))))

;;; Cursor colour. Batch Emacs has no graphical frame, so these tests
;;; make the frame count as graphical (the colour itself is then just
;;; the frame parameter 'cursor-color', which batch Emacs keeps).

(defmacro test-skg-zippy--with-graphical-frame-colored (color &rest body)
  "Run BODY with the selected frame counting as graphical, its cursor
colour COLOR, and, after it, the colour set back and zippy off."
  (declare (indent 1))
  `(let ((original-color (frame-parameter nil 'cursor-color)))
     (unwind-protect
         (cl-letf (((symbol-function 'skg-zippy--frame-can-show-cursor-color-p)
                    (lambda (_frame) t)))
           (set-frame-parameter nil 'cursor-color ,color)
           ,@body)
       (skg-zippy-mode -1)
       (set-frame-parameter nil 'cursor-color original-color)) ))

(defun test-skg-zippy--cursor-color ()
  (frame-parameter nil 'cursor-color))

(ert-deftest test-skg-zippy-cursor-is-green-only-in-skg-windows-and-restored ()
  "While zippy is on, the selected frame's cursor is green exactly when
its selected window shows an skg buffer, after a buffer switch (the
window hooks, run here by hand as redisplay would) and when zippy
comes on or off; elsewhere, and after zippy goes off, it is exactly the
colour it was. The hooks exist exactly while zippy is on."
  (test-skg-zippy--with-graphical-frame-colored "#ff69b4"
    (test-skg-zippy--with-buffers
        ((view (skg-content-view-mode))
         (plain (org-mode)))
      (switch-to-buffer view)
      (skg-zippy-mode 1)
      (should (equal (test-skg-zippy--cursor-color) skg-zippy-cursor-color))
      (should (memq #'skg-zippy--refresh-cursor-color-after-change
                    window-selection-change-functions))
      (should (memq #'skg-zippy--refresh-cursor-color-after-change
                    window-buffer-change-functions))
      (switch-to-buffer plain)
      (run-hook-with-args 'window-buffer-change-functions (selected-frame))
      (should (equal (test-skg-zippy--cursor-color) "#ff69b4"))
      (switch-to-buffer view)
      (run-hook-with-args 'window-buffer-change-functions (selected-frame))
      (should (equal (test-skg-zippy--cursor-color) skg-zippy-cursor-color))
      (let ((skg-zippy-cursor-color "yellow"))
        (skg-zippy--refresh-cursor-color-of-frame (selected-frame))
        (should (equal (test-skg-zippy--cursor-color) "yellow")))
      (skg-zippy-mode -1)
      (should (equal (test-skg-zippy--cursor-color) "#ff69b4"))
      (should-not (memq #'skg-zippy--refresh-cursor-color-after-change
                        window-selection-change-functions))
      (should-not (memq #'skg-zippy--refresh-cursor-color-after-change
                        window-buffer-change-functions))
      (should-not (frame-parameter nil 'skg-zippy-original-cursor-color)))))

(ert-deftest test-skg-zippy-cursor-color-turning-on-in-a-plain-buffer-changes-nothing ()
  "Zippy on while a non-skg buffer is selected leaves the colour alone,
and a buffer that becomes an skg buffer there turns it green."
  (test-skg-zippy--with-graphical-frame-colored "#ff69b4"
    (test-skg-zippy--with-buffers
        ((plain (org-mode)))
      (switch-to-buffer plain)
      (skg-zippy-mode 1)
      (should (equal (test-skg-zippy--cursor-color) "#ff69b4"))
      (skg-content-view-mode)
      (should (equal (test-skg-zippy--cursor-color) skg-zippy-cursor-color))
      (skg-zippy-mode -1)
      (should (equal (test-skg-zippy--cursor-color) "#ff69b4")))))

(ert-deftest test-skg-zippy-cursor-color-after-a-theme-changed-it ()
  "If something else changes the cursor colour while zippy's green is
on (a theme), zippy takes the new colour as the one to restore and
shows green again, and zippy's leaving restores the new colour."
  (test-skg-zippy--with-graphical-frame-colored "#ff69b4"
    (test-skg-zippy--with-buffers
        ((view (skg-content-view-mode)))
      (switch-to-buffer view)
      (skg-zippy-mode 1)
      (set-frame-parameter nil 'cursor-color "orange") ;; the theme
      (skg-zippy--refresh-cursor-color-after-change 'some-theme)
      (should (equal (test-skg-zippy--cursor-color) skg-zippy-cursor-color))
      (skg-zippy-mode -1)
      (should (equal (test-skg-zippy--cursor-color) "orange")))))

(ert-deftest test-skg-zippy-cursor-color-is-untouched-where-colour-cannot-be-set ()
  "On a frame that cannot show a cursor colour (a terminal), zippy
neither changes it nor errors."
  (test-skg-zippy--with-graphical-frame-colored "#ff69b4"
    (test-skg-zippy--with-buffers
        ((view (skg-content-view-mode)))
      (switch-to-buffer view)
      (cl-letf (((symbol-function 'skg-zippy--frame-can-show-cursor-color-p)
                 (lambda (_frame) nil)))
        (skg-zippy-mode 1)
        (should (equal (test-skg-zippy--cursor-color) "#ff69b4"))
        (skg-zippy-mode -1)
        (should (equal (test-skg-zippy--cursor-color) "#ff69b4"))))))

(provide 'test-skg-zippy-mode)
