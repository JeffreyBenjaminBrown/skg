;;; test-skg-repo-files-integrated.el --- Files the server integrated from disk, client side -*- lexical-binding: t; -*-

(defconst test-skg-repo-files-integrated--this-dir
  (file-name-directory load-file-name))

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-repo-files-integrated--this-dir))
(require 'cl-lib)
(require 'ert)
(load-file (expand-file-name "../../elisp/skg-init.el"
                             test-skg-repo-files-integrated--this-dir))

(defun test-skg-repo-files-integrated--kill-warnings-buffer ()
  (when (get-buffer skg-server-warnings-buffer-name)
    (kill-buffer skg-server-warnings-buffer-name)))

(defmacro test-skg-repo-files-integrated--with-views (views &rest body)
  "Run BODY with a buffer per (VIEW-ID . DIRTY) in VIEWS, DIRTY
meaning it has unsaved edits."
  (declare (indent 1))
  `(let ((buffers
          (mapcar (lambda (view)
                    (let ((buffer (generate-new-buffer "test-skg-view")))
                      (with-current-buffer buffer
                        (setq-local skg-view-id (car view))
                        (insert "* a headline\n")
                        (set-buffer-modified-p (cdr view)))
                      buffer))
                  ,views)))
     (unwind-protect (progn ,@body)
       (mapc #'kill-buffer buffers))))

(ert-deftest test-skg-repo-files-integrated-arrives-with-no-request-in-flight ()
  "The dispatcher hands the unprompted message to its own handler,
though no request registered one."
  (let* ((skg-response-handler-map nil)
         (skg-lp--pending-count 0)
         (handled nil)
         (skg-unprompted-response-handler-map
          `((repo-files-integrated
             . ,(lambda (_proc payload) (setq handled payload))))))
    (skg-lp--dispatch-by-type
     nil "((response-type repo-files-integrated) (integrated ()) (warnings ()) (view-ids ()))")
    (should (string-match-p "repo-files-integrated" handled))
    (should (= 0 skg-lp--pending-count))))

(ert-deftest test-skg-repo-files-integrated-is-registered-as-unprompted ()
  (should (eq (alist-get 'repo-files-integrated skg-unprompted-response-handler-map)
              #'skg--repo-files-integrated-handler)))

;; A repo-files-integrated message with INTEGRATED, WARNINGS and
;; '(view-ids ())', printed.
(defun test-skg-repo-files-integrated--payload (integrated warnings)
  (prin1-to-string
   `((response-type repo-files-integrated)
     (integrated ,integrated)
     (warnings ,warnings)
     (view-ids ()))))

(ert-deftest test-skg-repo-files-integrated-counts-alone-are-an-echo ()
  "A message with no warnings echoes its counts, one clause per
skgrepo; the warnings buffer is neither made nor shown."
  (test-skg-repo-files-integrated--kill-warnings-buffer)
  (let ((echoed nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (error "nothing may be shown")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq echoed (apply #'format format-string args)))))
      (skg--repo-files-integrated-handler
       nil
       (test-skg-repo-files-integrated--payload
        '((first 2) ("my main" 1)) '())))
    (should (equal echoed
                   "skg: repo first: 2 files integrated; repo my main: 1 file integrated"))
    (should-not (get-buffer skg-server-warnings-buffer-name))))

(ert-deftest test-skg-repo-files-integrated-real-warnings-go-to-the-warnings-buffer ()
  "A message carrying a warning goes, counts and warnings, to the
warnings buffer, which is shown."
  (test-skg-repo-files-integrated--kill-warnings-buffer)
  (unwind-protect
      (let ((shown nil))
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (setq shown buffer) nil)))
          (skg--repo-files-integrated-handler
           nil
           (test-skg-repo-files-integrated--payload
            '((first 2))
            '("Node bad-id changed on disk but could not be read."))))
        (should (equal (buffer-name shown) skg-server-warnings-buffer-name))
        (with-current-buffer skg-server-warnings-buffer-name
          (should (equal (buffer-string)
                         (concat "* Repo changes\n"
                                 "** repo first: 2 files integrated\n"
                                 "** Node bad-id changed on disk but could not be read.\n")))))
    (test-skg-repo-files-integrated--kill-warnings-buffer)))

(ert-deftest test-skg-repo-files-integrated-rerenders-the-clean-views-it-names ()
  "Only a named view without unsaved edits is rerendered: the request
excludes every other open view."
  (let ((sent nil)
        (skg--stream-in-progress nil))
    (test-skg-repo-files-integrated--with-views
        '(("named-clean" . nil) ("named-dirty" . t) ("unnamed" . nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (apply function args)))
                ((symbol-function 'skg-tcp-connect-to-rust)
                 (lambda () 'fake-proc))
                ((symbol-function 'process-send-string)
                 (lambda (_proc text) (setq sent text)))
                ((symbol-function 'skg--begin-stream) #'ignore)
                ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
                ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
                ((symbol-function 'skg--register-rerender-overPrivateText-confirmation)
                 #'ignore))
        (skg--repo-files-integrated-handler
         nil
         (prin1-to-string
          '((response-type repo-files-integrated)
            (integrated ((mine 1)))
            (warnings ())
            (view-ids (named-clean named-dirty))))))
      (should (string-match-p (regexp-quote "(request . \"rerender all views\")") sent))
      (should (string-match-p "exclude-view-ids" sent))
      (should (string-match-p "\"named-dirty\"" sent))
      (should (string-match-p "\"unnamed\"" sent))
      (should-not (string-match-p "\"named-clean\"" sent)))))

(ert-deftest test-skg-repo-files-integrated-sends-nothing-when-no-named-view-is-clean ()
  (let ((sent nil)
        (skg--stream-in-progress nil))
    (test-skg-repo-files-integrated--with-views
        '(("named-dirty" . t))
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (_proc text) (setq sent text))))
        (skg--rerender-views-changed-on-disk '("named-dirty" "not-open") 0)))
    (should (null sent))))

(ert-deftest test-skg-repo-files-integrated-waits-for-a-stream-in-flight ()
  (let ((scheduled nil)
        (skg--stream-in-progress "save"))
    (test-skg-repo-files-integrated--with-views
        '(("named-clean" . nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (time _repeat function &rest args)
                   (push (list time function args) scheduled)))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (error "nothing may be sent"))))
        (skg--rerender-views-changed-on-disk '("named-clean") 0)))
    (should (equal scheduled
                   `((,skg--repo-files-integrated-retry-seconds
                      skg--rerender-views-changed-on-disk
                      (("named-clean") 1)))))))
