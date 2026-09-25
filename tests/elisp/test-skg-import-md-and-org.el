;;; -*- lexical-binding: t; -*-
;;; Client flow for additive mixed Markdown/Org import.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'skg-request-import-md-and-org)

(ert-deftest test-skg-import-md-and-org-host-preview-approval-result ()
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0)
        (requests nil) (shown nil) (scheduled nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text requests)))
              ((symbol-function 'skg-big-nonfatal-message)
               (lambda (&rest args) (push args shown)))
              ((symbol-function 'read-string)
               (lambda (&rest _) "/host/notes"))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) t))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat callback &rest args)
                 (push (cons callback args) scheduled))))
      (skg-import-md-and-org "/container/notes" "private")
      (should (string-match-p "input-directory . \"/container/notes\"" (car requests)))
      (funcall (cadr (assoc 'import-md-and-org-host-mapping-needed
                           skg-response-handler-map))
               'fake-proc
               "((content \"Host mapping needed\"))")
      (let ((retry (pop scheduled)))
        (apply (car retry) (cdr retry)))
      (should (string-match-p "host-root . \"/host/notes\"" (car requests)))
      (funcall (cadr (assoc 'import-md-and-org-preview
                           skg-response-handler-map))
               'fake-proc
               "((content \"Valid preview\") (approval-token \"opaque\"))")
      (let ((apply-request (pop scheduled)))
        (apply (car apply-request) (cdr apply-request)))
      (should (string-match-p "approval-token . \"opaque\"" (car requests)))
      (funcall (cadr (assoc 'import-md-and-org-result
                           skg-response-handler-map))
               'fake-proc
               "((content \"Imported\") (record-id \"record\"))")
      (should (equal (car (last (car shown))) "Imported"))
      (should (null skg-response-handler-map))
      (should (= skg-lp--pending-count 0))
      (should (eq (caar scheduled) #'skg--import-rerender-clean-views)))))

(ert-deftest test-skg-import-md-and-org-decline-cancels ()
  (let ((skg-response-handler-map nil) (requests nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text requests)))
              ((symbol-function 'skg-big-nonfatal-message) #'ignore)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (skg-import-md-and-org "/container/notes" "private")
      (funcall (cadr (assoc 'import-md-and-org-preview
                           skg-response-handler-map))
               'fake-proc
               "((content \"Valid preview\") (approval-token \"opaque\"))")
      (should (string-match-p "action . \"cancel\"" (car requests)))
      (funcall (cadr (assoc 'import-md-and-org-result
                           skg-response-handler-map))
               'fake-proc "((content \"Cancelled\"))")
      (should (null skg-response-handler-map)))))

(ert-deftest test-skg-import-rerender-excludes-dirty-views ()
  (let ((sent nil))
    (with-temp-buffer
      (setq-local skg-view-uri "dirty-uri")
      (insert "unsaved text")
      (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                 (lambda () 'fake-proc))
                ((symbol-function 'process-send-string)
                 (lambda (_proc text) (setq sent text)))
                ((symbol-function 'skg--begin-stream) #'ignore)
                ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
                ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
                ((symbol-function 'skg--register-rerender-overPrivateText-confirmation)
                 #'ignore))
        (skg--import-rerender-clean-views)
        (should (string-match-p
                 (regexp-quote "(exclude-view-uris \"dirty-uri\")") sent))))))
