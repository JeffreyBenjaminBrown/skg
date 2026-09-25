;;; -*- lexical-binding: t; -*-
;;; Real-server import, preview confirmation, render, and save.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")
(require 'cl-lib)

(setq skg-port (string-to-number (getenv "SKG_TEST_PORT")))
(run-at-time 30 nil (lambda () (message "Import integration timed out")
                      (kill-emacs 1)))

(let ((host-prompts 0)
      (approvals 0)
      (source-dir (getenv "SKG_TEST_SOURCE_DIR")))
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _)
               (cl-incf host-prompts)
               ""))
            ((symbol-function 'yes-or-no-p)
             (lambda (&rest _)
               (cl-incf approvals)
               t)))
    (skg-import-md-and-org (getenv "SKG_TEST_INPUT_DIR") "main")
    (unless (skg-test-wait-for
             (lambda ()
               (let ((buf (get-buffer "*skg import result*")))
                 (and buf (with-current-buffer buf
                            (string-match-p "Imported [0-9]+ nodes"
                                            (buffer-string))))) )
             15)
      (error "No successful import result; preview=%S result=%S host-prompts=%d approvals=%d"
             (let ((buf (get-buffer "*skg import preview*")))
               (when buf (with-current-buffer buf (buffer-string))))
             (let ((buf (get-buffer "*skg import result*")))
               (when buf (with-current-buffer buf (buffer-string))))
             host-prompts approvals)))
  (unless (= host-prompts 1) (error "Expected one host-root prompt"))
  (unless (= approvals 1) (error "Expected one explicit approval"))
  (unless (file-exists-p (expand-file-name "import-root.skg" source-dir))
    (error "Explicit Org root ID was not published"))
  ;; The import result schedules an asynchronous refresh. Let that finish
  ;; before issuing the next request on the same length-prefixed connection.
  (accept-process-output nil 0.1)
  (unless (skg-test-wait-for-response 15)
    (error "Import refresh did not settle"))
  (skg-request-single-root-content-view-from-id "import-root")
  (let ((view (skg-test-wait-for-buffer "*Sample*" 15)))
    (unless view (error "Imported root view did not open"))
    ;; Link-status annotations also issue asynchronous requests on open.
    (unless (skg-test-wait-for-response 15)
      (error "Imported view did not settle"))
    (with-current-buffer view
      (unless (string-match-p ",\\* not a heading" (buffer-string))
        (error "Literal heading was not safely encoded in the view"))
      (skg-request-save-buffer)
      (unless (skg-test-wait-for-response 15)
        (error "Saving imported view timed out"))))
  (let ((files (directory-files source-dir nil "\\.skg$")))
    (unless (= (length files) 7)
      (error "Literal heading became a node: expected 7 files, got %d"
             (length files)))
    (unless (cl-some
             (lambda (file)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name file source-dir))
                 (goto-char (point-min))
                 (search-forward
                  (concat "First line" (make-string 2 ?\\) "\n")
                  nil t)))
             files)
      (error "Markdown hard break did not become an Org double-backslash break")))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "import-root.skg" source-dir))
    (unless (string-match-p "\\* not a heading" (buffer-string))
      (error "Literal heading was lost from the saved root")))
  (message "PASS: mixed import, approval, literal body, save")
  (kill-emacs 0))
