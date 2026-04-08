;;; test-skg-insert-heading.el --- Test C-return from a root headline

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-sexpr-edit)
(require 'skg-config)

(defun test--with-skg-content-view (org-text config-text body-fn)
  "Run BODY-FN in a temp skg content-view buffer with ORG-TEXT.
CONFIG-TEXT is written to a temporary skgconfig.toml so that
skg-config-dir is set and skg--owned-sources works."
  (let* ((config-dir (make-temp-file "skg-test-config" t))
         (config-file (expand-file-name "skgconfig.toml" config-dir))
         (skg-config-dir (file-name-as-directory config-dir)))
    (with-temp-file config-file
      (insert config-text))
    (unwind-protect
        (with-temp-buffer
          (insert org-text)
          (skg-content-view-mode)
          (goto-char (point-min))
          (funcall body-fn))
      (delete-file config-file)
      (delete-directory config-dir))))

(defvar test--config-two-owned-sources
  (concat "[[sources]]\n"
          "name = \"public\"\n"
          "path = \"/tmp\"\n"
          "user_owns_it = true\n\n"
          "[[sources]]\n"
          "name = \"private\"\n"
          "path = \"/tmp\"\n"
          "user_owns_it = true\n")
  "Config text with two owned sources.")

(ert-deftest test-insert-heading-at-root ()
  "C-return on a root headline prompts for source in the minibuffer
and inserts metadata with the chosen source on the new headline."
  (test--with-skg-content-view
   "* (skg (node (id a) (source public))) a\n"
   test--config-two-owned-sources
   (lambda ()
     (let ((source-buffer (current-buffer)))

       ;; Precondition: point on the only headline.
       (should (org-at-heading-p))
       (should (= (org-outline-level) 1))

       ;; C-return: inserts a new heading and prompts for source.
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (_prompt _coll &rest _) "private")))
         (org-insert-heading-respect-content))

       ;; 1. The source buffer now has two level-1 headlines.
       (with-current-buffer source-buffer
         (let ((content (buffer-substring-no-properties
                         (point-min) (point-max))))
           ;; Original headline unchanged.
           (should (string-match-p
                    "^\\* (skg (node (id a) (source public))) a$"
                    content))
           ;; New headline has metadata with chosen source.
           (should (string-match-p
                    "^\\* (skg (node (source private))) $"
                    content))
           (should (= 2 (how-many "^\\* " (point-min) (point-max))))))

       ;; 2. No sexp-edit buffer was opened.
       (should-not
        (cl-find-if
         (lambda (b)
           (buffer-local-value 'skg-sexp-edit--source-buffer b))
         (buffer-list)))))))
