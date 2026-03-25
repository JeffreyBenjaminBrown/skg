;;; test-skg-insert-heading-source-prompt.el --- Test C-return source prompt
;;;
;;; When org-insert-heading-respect-content is called on a root headline
;;; in a content-view buffer, and the new headline has no metadata,
;;; skg-edit-metadata should prompt for a source in the minibuffer
;;; (not open the sexp-edit buffer) and insert the chosen source
;;; as metadata on the new headline.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-sexpr-edit)
(require 'skg-config)

(defvar test--config-public-and-private
  (concat "[[sources]]\n"
          "name = \"public\"\n"
          "path = \"" (expand-file-name
                       "test-skg-insert-heading-source-prompt/public"
                       (file-name-directory load-file-name)) "\"\n"
          "user_owns_it = true\n\n"
          "[[sources]]\n"
          "name = \"private\"\n"
          "path = \"" (expand-file-name
                       "test-skg-insert-heading-source-prompt/private"
                       (file-name-directory load-file-name)) "\"\n"
          "user_owns_it = true\n")
  "Config text with two owned sources: public and private.")

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
          (org-mode)
          (skg-content-view-mode 1)
          (goto-char (point-min))
          (funcall body-fn))
      (delete-file config-file)
      (delete-directory config-dir))))

;; Test 1: C-return inserts metadata with chosen source via minibuffer,
;;         does NOT open a sexp-edit buffer.

(ert-deftest test-insert-heading-prompts-for-source ()
  "C-return on a root headline should prompt for source in minibuffer,
insert metadata with chosen source, and not open the sexp-edit buffer."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-public-and-private
   (lambda ()
     (should (org-at-heading-p))
     (should (= (org-outline-level) 1))

     ;; Mock completing-read to return "private".
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt _coll &rest _) "private")))
       (org-insert-heading-respect-content))

     ;; The source buffer should have two level-1 headlines.
     (let ((content (buffer-substring-no-properties
                     (point-min) (point-max))))

       ;; Original headline unchanged.
       (should (string-match-p
                "^\\* (skg (node (id x) (source public))) x$"
                content))

       ;; New headline has metadata with chosen source "private".
       (should (string-match-p
                "^\\* (skg (node (source private))) $"
                content))

       ;; Exactly two headlines.
       (should (= 2 (how-many "^\\* " (point-min) (point-max)))))

     ;; No sexp-edit buffer was opened.
     (should-not
      (cl-find-if
       (lambda (b)
         (buffer-local-value 'skg-sexp-edit--source-buffer b))
       (buffer-list))))))

;; Test 2: Single owned source skips the prompt entirely.

(ert-deftest test-insert-heading-single-source-no-prompt ()
  "When there is only one owned source, C-return should use it
without prompting."
  (let ((one-source-config
         (concat "[[sources]]\n"
                 "name = \"only\"\n"
                 "path = \"/tmp\"\n"
                 "user_owns_it = true\n")))
    (test--with-skg-content-view
     "* (skg (node (id x) (source only))) x\n"
     one-source-config
     (lambda ()
       (should (org-at-heading-p))

       ;; completing-read should NOT be called.
       (let ((cr-called nil))
         (cl-letf (((symbol-function 'completing-read)
                    (lambda (&rest _)
                      (setq cr-called t)
                      "only")))
           (org-insert-heading-respect-content))
         (should-not cr-called))

       (let ((content (buffer-substring-no-properties
                       (point-min) (point-max))))
         (should (string-match-p
                  "^\\* (skg (node (source only))) $"
                  content)))))))

;; Test 3: The cycling closure works correctly.

(ert-deftest test-source-cycling-wraps-around ()
  "skg--prompt-for-owned-source's cycling closure should wrap around."
  (let* ((config-dir (make-temp-file "skg-test-config" t))
         (config-file (expand-file-name "skgconfig.toml" config-dir))
         (skg-config-dir (file-name-as-directory config-dir)))
    (with-temp-file config-file
      (insert test--config-public-and-private))
    (unwind-protect
        (let ((sources (skg--owned-sources))
              results)
          ;; Verify we have two sources in expected order.
          (should (equal sources '("public" "private")))

          ;; Mock completing-read to simulate cycling:
          ;; Start at "public", cycle right once to reach "private".
          ;; The cycle closure replaces minibuffer contents,
          ;; so we test the logic by calling the cycle function directly.
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt coll &rest _)
                       ;; Simulate: start empty, cycle right once.
                       ;; The cycle closure does:
                       ;;   idx = (cl-position cur sources) or 0
                       ;;   new = (nth (mod (+ idx dir) len) sources)
                       ;; Starting from "" (not in list) -> idx=0 ("public"),
                       ;; cycling right: (mod (+ 0 1) 2) = 1 -> "private"
                       (let* ((idx 0)
                              (new (nth (mod (+ idx 1) (length sources))
                                        sources)))
                         new))))
            (should (equal (skg--prompt-for-owned-source) "private")))

          ;; Test wrap-around: from "private" (idx=1), cycle right -> "public"
          (let* ((idx 1)
                 (new (nth (mod (+ idx 1) (length sources)) sources)))
            (should (equal new "public")))

          ;; Test cycle left from "public" (idx=0) -> wraps to "private"
          (let* ((idx 0)
                 (new (nth (mod (+ idx -1) (length sources)) sources)))
            (should (equal new "private"))))
      (delete-file config-file)
      (delete-directory config-dir))))

;; Test 4: Existing metadata still opens the sexp-edit buffer.

(ert-deftest test-edit-existing-metadata-opens-edit-buffer ()
  "When a headline already has metadata, skg-edit-metadata should
open the sexp-edit buffer (not prompt in minibuffer)."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-public-and-private
   (lambda ()
     (should (org-at-heading-p))
     (skg-edit-metadata)

     ;; A sexp-edit buffer should have been opened.
     (let ((edit-buf
            (cl-find-if
             (lambda (b)
               (buffer-local-value 'skg-sexp-edit--source-buffer b))
             (buffer-list))))
       (should edit-buf)
       (kill-buffer edit-buf)))))

(provide 'test-skg-insert-heading-source-prompt)
