;;; test-skg-server-warnings.el --- The server's startup warnings, client side -*- lexical-binding: t; -*-

(defconst test-skg-server-warnings--this-dir
  (file-name-directory load-file-name))

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-server-warnings--this-dir))
(require 'cl-lib)
(require 'ert)
(load-file (expand-file-name "../../elisp/skg-init.el"
                             test-skg-server-warnings--this-dir))
(require 'skg-request-rerender-all-views) ; for skg--register-rerender-stream-handlers

(defun test-skg-server-warnings--kill-buffer ()
  (when (get-buffer skg-server-warnings-buffer-name)
    (kill-buffer skg-server-warnings-buffer-name)))

(ert-deftest test-skg-a-new-connection-asks-for-the-server-warnings ()
  "The request goes out once, when the connection is made."
  (let* ((server (make-network-process
                  :name "test-skg-warnings-server" :server t
                  :host "127.0.0.1" :service t))
         (skg-port (process-contact server :service))
         (skg-rust-tcp-proc nil)
         (skg-response-handler-map nil)
         (skg-lp--pending-count 0)
         (sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc text) (push text sent))))
          (skg-tcp-connect-to-rust)
          (skg-tcp-connect-to-rust) ; already connected: sends nothing
          (should (equal sent '("((request . \"server warnings\"))\n")))
          (should (assoc 'server-warnings skg-response-handler-map)))
      (when (process-live-p skg-rust-tcp-proc)
        (delete-process skg-rust-tcp-proc))
      (delete-process server))))

(ert-deftest test-skg-nonempty-server-warnings-open-their-buffer ()
  (test-skg-server-warnings--kill-buffer)
  (unwind-protect
      (progn
        (skg--server-warnings-handler
         nil
         (prin1-to-string
          '((response-type server-warnings)
            (warnings ("skgconfig.toml sets 'max_role_tree_depth', a key skg does not know, so it has no effect. Its current spelling is 'max_roleTree_depth'.")))))
        (with-current-buffer skg-server-warnings-buffer-name
          (should (derived-mode-p 'org-mode))
          (should (string-match-p "^\\* skgconfig.toml sets 'max_role_tree_depth'"
                                  (buffer-string)))
          (should (string-match-p "max_roleTree_depth" (buffer-string)))))
    (test-skg-server-warnings--kill-buffer)))

(ert-deftest test-skg-empty-server-warnings-open-nothing ()
  (test-skg-server-warnings--kill-buffer)
  (skg--server-warnings-handler
   nil "((response-type server-warnings) (warnings ()))")
  (should-not (get-buffer skg-server-warnings-buffer-name)))

;;; Buffer corrections: the server's, per view, appended to the same buffer.

(defconst test-skg-server-warnings--correction-of-new-child
  (concat "Drew what the graph says belongs here.\n"
          "*** new child\n"
          "- viewnode ID: 0d. To find it: C-c g v 0d\n")
  "A buffer correction as the server writes it, naming one headline.")

(defconst test-skg-server-warnings--correction-of-stray
  (concat "Removed what the graph says does not belong here.\n"
          "*** stray\n"
          "- viewnode ID: 0e. To find it: C-c g v 0e\n")
  "Another buffer correction, naming another headline.")

(defun test-skg-server-warnings--with-view-buffer (name view-id body)
  "Call BODY with a buffer NAME whose `skg-view-id' is VIEW-ID."
  (let ((buffer (generate-new-buffer name)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local skg-view-id view-id))
          (funcall body buffer))
      (kill-buffer buffer))))

(defmacro test-skg-server-warnings--quietly (&rest body)
  "Run BODY with display and the echo area stubbed; return what BODY
showed in the echo area, a list of strings, newest first."
  (declare (indent 0))
  `(let ((echoed nil))
     (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil))
               ((symbol-function 'message)
                (lambda (format-string &rest args)
                  (when format-string
                    (push (apply #'format format-string args) echoed)))))
       ,@body)
     echoed))

(ert-deftest test-skg-corrections-append-one-headline-per-view ()
  "One level-1 headline per view: its buffer's name when a buffer has
the view ID, else the ID. Each message a level-2 headline over its own
level-3 one. All of it after what the buffer held."
  (test-skg-server-warnings--kill-buffer)
  (unwind-protect
      (test-skg-server-warnings--with-view-buffer
       "*test corrected view*" "content-view:5"
       (lambda (_view-buffer)
         (with-current-buffer
             (get-buffer-create skg-server-warnings-buffer-name)
           (insert "* an earlier startup warning"))
         (let ((echoed
                (test-skg-server-warnings--quietly
                  (skg-show-corrections-in-server-warnings
                   (car (read-from-string
                         (concat
                          "((content-view:5 "
                          (prin1-to-string
                           test-skg-server-warnings--correction-of-new-child)
                          ") (\"search:two words\" "
                          (prin1-to-string
                           test-skg-server-warnings--correction-of-stray)
                          " \"Kept as it was.\"))")))))))
           (should (equal (car echoed)
                          (concat "skg: the server corrected 3 headlines"
                                  " in 2 views - see *SKG Server Warnings*"))))
         (with-current-buffer skg-server-warnings-buffer-name
           (should (derived-mode-p 'org-mode))
           (should
            (equal
             (buffer-string)
             (concat
              "* an earlier startup warning\n"
              "* Buffer corrections in *test corrected view*\n"
              "** " test-skg-server-warnings--correction-of-new-child
              "* Buffer corrections in search:two words\n"
              "** " test-skg-server-warnings--correction-of-stray
              "** Kept as it was.\n")))
           (goto-char (point-min))
           (search-forward "\n*** stray")
           (should (org-up-heading-safe))
           (should (looking-at-p "\\*\\* Removed what the graph"))
           (should (org-up-heading-safe))
           (should (looking-at-p "\\* Buffer corrections in search:two words$")))))
    (test-skg-server-warnings--kill-buffer)))

(ert-deftest test-skg-empty-corrections-open-nothing ()
  (test-skg-server-warnings--kill-buffer)
  (should-not
   (test-skg-server-warnings--quietly
     (skg-show-corrections-in-server-warnings nil)
     (skg-show-corrections-in-server-warnings '())))
  (should-not (get-buffer skg-server-warnings-buffer-name)))

(defun test-skg-server-warnings--save-result (save-buffer payload)
  "Run the save-result handler on PAYLOAD for SAVE-BUFFER, stubbing
the stream, the locks, the buffer replacement and the display."
  (let ((skg-response-handler-map nil))
    (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
              ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
              ((symbol-function 'skg-replace-buffer-with-new-content)
               #'ignore)
              ((symbol-function 'skg-big-nonfatal-message) #'ignore))
      (test-skg-server-warnings--quietly
        (skg--save-result-handler save-buffer payload)))))

(ert-deftest test-skg-save-result-corrections-name-the-collateral-view ()
  (test-skg-server-warnings--kill-buffer)
  (unwind-protect
      (test-skg-server-warnings--with-view-buffer
       "*test saved view*" "content-view:1"
       (lambda (save-buffer)
         (test-skg-server-warnings--with-view-buffer
          "*test collateral view*" "content-view:7"
          (lambda (_collateral-buffer)
            (test-skg-server-warnings--save-result
             save-buffer
             (prin1-to-string
              `((response-type save-result)
                (content "* saved\n")
                (errors ())
                (warnings ())
                (corrections
                 ((content-view:7
                   ,test-skg-server-warnings--correction-of-new-child)))
                (point-lines-below-focused-headline 0))))
            (with-current-buffer skg-server-warnings-buffer-name
              (should
               (equal (buffer-string)
                      (concat
                       "* Buffer corrections in *test collateral view*\n"
                       "** "
                       test-skg-server-warnings--correction-of-new-child))))))))
    (test-skg-server-warnings--kill-buffer)))

(ert-deftest test-skg-save-result-without-corrections-opens-nothing ()
  (test-skg-server-warnings--kill-buffer)
  (test-skg-server-warnings--with-view-buffer
   "*test saved view*" "content-view:1"
   (lambda (save-buffer)
     (test-skg-server-warnings--save-result
      save-buffer
      "((response-type save-result) (content \"* saved\\n\") (errors ()) (warnings ()))")
     (test-skg-server-warnings--save-result
      save-buffer
      "((response-type save-result) (content \"* saved\\n\") (errors ()) (warnings ()) (corrections ()))")
     (test-skg-server-warnings--save-result
      save-buffer
      "((response-type save-result) (content nil) (errors (\"refused\")) (warnings ()))")))
  (should-not (get-buffer skg-server-warnings-buffer-name)))

(ert-deftest test-skg-rerender-done-appends-corrections ()
  (test-skg-server-warnings--kill-buffer)
  (unwind-protect
      (let ((skg-response-handler-map nil))
        (with-current-buffer
            (get-buffer-create skg-server-warnings-buffer-name)
          (insert "* an earlier correction\n"))
        (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
                  ((symbol-function 'skg--unlock-all-save-locked) #'ignore))
          (skg--register-rerender-stream-handlers)
          (let ((handler (cadr (assoc 'rerender-done
                                      skg-response-handler-map))))
            (test-skg-server-warnings--quietly
              (funcall handler
                       nil
                       (prin1-to-string
                        `((response-type rerender-done)
                          (errors ())
                          (warnings ())
                          (corrections
                           (("search:two words"
                             ,test-skg-server-warnings--correction-of-stray)))))))))
        (with-current-buffer skg-server-warnings-buffer-name
          (should
           (equal (buffer-string)
                  (concat "* an earlier correction\n"
                          "* Buffer corrections in search:two words\n"
                          "** "
                          test-skg-server-warnings--correction-of-stray)))))
    (test-skg-server-warnings--kill-buffer)))

(provide 'test-skg-server-warnings)
