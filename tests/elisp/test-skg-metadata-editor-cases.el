;;; test-skg-metadata-editor-cases.el --- The metadata editor's shared cases

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-metadata)
(require 'skg-request-server-warnings)
(require 'skg-sexpr-edit)

;; Each case of the file, which the Neovim tests read too: an alist
;; from each field's key (a string) to its value, a string; a block's
;; lines are joined by newlines. The key "name" holds the case's name.
(defconst test-skg-metadata-editor--cases-file
  (expand-file-name "../shared/metadata-editor-cases.txt"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun test-skg-metadata-editor--cases ()
  "Parse `test-skg-metadata-editor--cases-file'."
  (let ((cases nil) (case nil) (block-key nil))
    (dolist (line (split-string
                   (with-temp-buffer
                     (insert-file-contents test-skg-metadata-editor--cases-file)
                     (buffer-string))
                   "\n"))
      (cond ((string-prefix-p "==== " line)
             (when case (push (nreverse case) cases))
             (setq case (list (cons "name" (substring line 5)))
                   block-key nil))
            ((string-match "\\`---- \\([^:]+\\):\\(?: \\(.*\\)\\)?\\'" line)
             (let ((key (match-string 1 line))
                   (value (match-string 2 line)))
               (setq block-key (unless value key))
               (push (cons key (or value "")) case)))
            ((and block-key (not (string-empty-p line)))
             (let ((entry (assoc block-key case)))
               (setcdr entry (if (string-empty-p (cdr entry))
                                 line
                               (concat (cdr entry) "\n" line)))))))
    (when case (push (nreverse case) cases))
    (nreverse cases)))

(defun test-skg-metadata-editor--field (case key)
  "The value of CASE's field KEY, or nil."
  (cdr (assoc key case)))

(ert-deftest test-skg-metadata-editor-shared-cases ()
  "The editor shows each case's metadata as its fill-in, saves it
untouched as its strip, and the request writers place a request as
the case says; the buffer corrections display as the case says. The
cases are shared with Neovim."
  (let ((cases (test-skg-metadata-editor--cases)))
    (should (> (length cases) 10))
    (dolist (case cases)
      (let ((name (test-skg-metadata-editor--field case "name"))
            (metadata (test-skg-metadata-editor--field case "metadata"))
            (corrections (test-skg-metadata-editor--field case "corrections")))
        (when metadata
          (let* ((sexp (car (read-from-string metadata)))
                 (shown (skg-sexp-edit--org-text-to-show
                         sexp (test-skg-metadata-editor--field case "title"))))
            (should (equal (list name shown)
                           (list name (test-skg-metadata-editor--field
                                       case "fill-in"))))
            (should (equal (list name (skg-sexp-edit--metadata-text-to-save
                                       shown
                                       (skg-metadata-fields-kind-of-sexp sexp)))
                           (list name (test-skg-metadata-editor--field
                                       case "strip"))))
            (dolist (writer `(("edit request" "with the edit request"
                               ,#'skg--metadata-with-edit-request)
                              ("readiness request" "with the readiness request"
                               ,#'skg--metadata-with-readiness-request)))
              (let ((request (test-skg-metadata-editor--field
                              case (nth 0 writer))))
                (when request
                  (should (equal
                           (list name (prin1-to-string
                                       (funcall (nth 2 writer) sexp
                                                (car (read-from-string request)))))
                           (list name (test-skg-metadata-editor--field
                                       case (nth 1 writer))))))))))
        (when corrections
          (should (equal (list name (string-trim-right
                                     (skg--corrections-to-org-string
                                      (car (read-from-string corrections)))))
                         (list name (test-skg-metadata-editor--field
                                     case "display")))))))))

(provide 'test-skg-metadata-editor-cases)
;;; test-skg-metadata-editor-cases.el ends here
