;;; test-skg-close-all.el --- Tests for skg-close-all-skg-buffers

;;; PURPOSE: Guards against a regression where, after `skg-reload',
;;; `skg-close-all-skg-buffers' failed to close any buffers.
;;;
;;; The previous root cause was that `unload-feature' cleared each
;;; buffer's `skg-view-uri' (the var was declared in the unloaded
;;; file) and unbound the major-mode function (so the buffer's
;;; major-mode degraded to `org-mode'). `skg-reload' now omits
;;; `skg-buffer' from the unload list and re-loads it plainly
;;; instead, preserving each buffer's mode and view-uri. Those two
;;; signals are all `skg-buffer-p' relies on, so a user's own
;;; .skg.org file (org-mode, no view-uri) is never reaped, even
;;; when its first heading begins with `(skg'.

(defconst test-skg-close-all--this-dir
  (file-name-directory load-file-name)
  "Directory of this test file, captured at load time because
`load-file-name' is nil when ERT runs the test bodies.")

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-close-all--this-dir))
(require 'ert)
(load-file (expand-file-name "../../elisp/skg-init.el"
                             test-skg-close-all--this-dir))

(defun test-skg-close-all--make-fake-skg-buffer (name)
  "Create a buffer named NAME that looks like a real skg content view."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "* (skg (node (id fake-id) (source main))) fake title\n")
      (skg-content-view-mode)
      (setq skg-view-uri (concat "test-uri-" name))
      (set-buffer-modified-p nil))
    buf))

(ert-deftest test-skg-close-all-baseline ()
  "Close-all closes buffers with skg-view-uri and skg mode."
  (let ((buf (test-skg-close-all--make-fake-skg-buffer
              "*fake-skg-baseline*")))
    (should (buffer-live-p buf))
    (skg-close-all-skg-buffers)
    (should-not (buffer-live-p buf))))

(ert-deftest test-skg-reload-preserves-mode-and-view-uri ()
  "`skg-reload' must not downgrade open skg buffers to org-mode
nor clear their `skg-view-uri'. Before the fix,
`unload-feature' on `skg-buffer' did exactly that."
  (load-file (expand-file-name
              "../../elisp/skg-reload.el"
              test-skg-close-all--this-dir))
  (let ((buf (test-skg-close-all--make-fake-skg-buffer
              "*fake-skg-preserved*")))
    (unwind-protect
        (progn
          (skg-reload)
          (should (buffer-live-p buf))
          (should (with-current-buffer buf
                    (derived-mode-p 'skg-content-view-mode)))
          (should (string= (buffer-local-value 'skg-view-uri buf)
                           "test-uri-*fake-skg-preserved*")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-skg-reload-refreshes-keymap-bindings ()
  "Edits to bindings in `skg-keymaps-and-aliases.el' must take
effect on reload. Before the fix, `defvar MAP (let ((m …)) …)'
only ran its initialiser on the first load, so bindings encoded
in later loads of the file were silently dropped."
  (load-file (expand-file-name
              "../../elisp/skg-reload.el"
              test-skg-close-all--this-dir))
  ;; Simulate: a user held a binding in the map, then edited the
  ;; file somehow (which we can't really do in a test). Instead:
  ;; clobber the binding in memory, then reload, and assert the
  ;; binding is back. This passes iff reload re-populates the
  ;; keymap from the file on disk.
  (define-key skg-content-view-mode-map
    (kbd "C-c g RET") 'clobbered-sentinel)
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c g RET"))
              'clobbered-sentinel))
  (skg-reload)
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c g RET"))
              #'skg-goto)))

(ert-deftest test-skg-close-all-spares-org-buffer-with-skg-heading ()
  "A real file the user is editing must survive `skg-close-all-skg-buffers',
even when it is an org file whose first heading begins with `(skg'
\(e.g. a .skg.org export). Such a buffer has no skg-view-uri and is
in plain `org-mode', so `skg-buffer-p' must not claim it."
  (let* ((tmp (make-temp-file "skg-spare-" nil ".skg.org"))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (insert "* (skg (node (id real-file-id) (source main))) real title\n")
            (org-mode)
            (save-buffer))
          (should (buffer-live-p buf))
          (should-not (skg-buffer-p buf))
          (skg-close-all-skg-buffers)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file tmp))))

(provide 'test-skg-close-all)
