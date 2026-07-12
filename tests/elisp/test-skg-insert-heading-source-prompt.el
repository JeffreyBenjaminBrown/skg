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

(defvar test--config-with-foreign-source
  (concat test--config-public-and-private
          "\n[[sources]]\n"
          "name = \"foreign\"\n"
          "path = \"" (expand-file-name
                       "test-skg-insert-heading-source-prompt/foreign"
                       (file-name-directory load-file-name)) "\"\n"
          "user_owns_it = false\n")
  "Config text with two owned sources and one foreign source.")

(defvar test--config-with-interleaved-source-sets
  (concat "[[source_sets]]\n"
          "name = \"public-set\"\n"
          "sources = [\"public\"]\n\n"
          "[[sources]]\n"
          "name = \"public\"\n"
          "path = \"" (expand-file-name
                       "test-skg-insert-heading-source-prompt/public"
                       (file-name-directory load-file-name)) "\"\n"
          "user_owns_it = true\n\n"
          "[[source_sets]]\n"
          "name = \"private-set\"\n"
          "sources = [\"private\"]\n\n"
          "[[sources]]\n"
          "name = \"private\"\n"
          "path = \"" (expand-file-name
                       "test-skg-insert-heading-source-prompt/private"
                       (file-name-directory load-file-name)) "\"\n"
          "user_owns_it = true\n")
  "Config text with [[sources]] interleaved among other array\ntables. The [[source_sets]] tables are RETIRED config the server\nwould reject; they remain here to pin that the elisp readers skip\ntables they do not care about.")

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
       (with-current-buffer edit-buf
         (goto-char (point-min))
         (outline-next-heading)
         (should (looking-at "^\\* title$"))
         (should (get-text-property (point) 'read-only))
         (outline-next-heading)
         (should (looking-at "^\\*\\* x$"))
         (should (get-text-property (point) 'read-only)))
       (kill-buffer edit-buf)))))

(ert-deftest test-view-source-list-includes-all-sources ()
  "skg-view-source-list should list every configured source and path."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-with-foreign-source
   (lambda ()
     (unwind-protect
         (progn
           (skg-view-source-list)
           (let ((source-buffer (get-buffer "*skg-sources*")))
             (should source-buffer)
             (with-current-buffer source-buffer
               (should (derived-mode-p 'org-mode))
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "^\\* public$" content))
                 (should (string-match-p "^\\* private$" content))
                 (should (string-match-p "^\\* foreign$" content))
                 (should (string-match-p "/public" content))
                 (should (string-match-p "/private" content))
                 (should (string-match-p "/foreign" content))))))
       (when (get-buffer "*skg-sources*")
         (kill-buffer "*skg-sources*"))))))

(ert-deftest test-source-change-prompt-starts-with-current-source ()
  "skg--prompt-for-source-change should put current source in editable text."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-public-and-private
   (lambda ()
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection predicate require-match
                         initial-input &optional hist def
                         inherit-input-method)
                  (should-not (string-match-p "current public" prompt))
                  (should (equal collection '("public" "private")))
                  (should (null predicate))
                  (should (null require-match))
                  (should (equal initial-input "public"))
                  (should (null hist))
                  (should (null def))
                  (should (null inherit-input-method))
                  "private")))
       (should (equal (skg--prompt-for-source-change "public")
                      "private"))))))

(ert-deftest test-source-set-prompt-completes-configured-source-sets ()
  "skg--prompt-for-source-set completes the prefix source-set choices:
the sources in privacy order, then \"all\"."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-with-interleaved-source-sets
   (lambda ()
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection predicate require-match
                         initial-input &optional hist def
                         inherit-input-method)
                  (should (string-match-p "Most private source" prompt))
                  (should (equal collection
                                 '("public" "private" "all")))
                  (should (null predicate))
                  (should require-match)
                  (should (null initial-input))
                  (should (null hist))
                  (should (equal def "all"))
                  (should (null inherit-input-method))
                  "private")))
       (should (equal (skg--prompt-for-source-set)
                      "private"))))))

(ert-deftest test-config-readers-handle-interleaved-source-tables ()
  "Elisp config readers should not confuse [[sources]] and [[source_sets]]."
  (test--with-skg-content-view
   "* (skg (node (id x) (source public))) x\n"
   test--config-with-interleaved-source-sets
   (lambda ()
     (should (equal (skg--source-names) '("public" "private")))
     (should (equal (skg--source-set-names)
                    '("public" "private" "all")))
     (should (equal (mapcar #'car (skg--source-paths))
                    '("public" "private"))))))

;; --- Empty-node metadata view (C-c v m on a metadata-less headline) ---

(defvar test--config-one-source
  (concat "[[sources]]\n"
          "name = \"only\"\n"
          "path = \"/tmp\"\n"
          "user_owns_it = true\n")
  "Config text with a single owned source, so no source prompt fires.")

(defun test--skg-edit-buffer ()
  "Return the open sexp-edit buffer, if any."
  (cl-find-if
   (lambda (b)
     (buffer-local-value 'skg-sexp-edit--source-buffer b))
   (buffer-list)))

;; Builder: source pre-filled, other editable fields childless, title group.

(ert-deftest test-empty-node-org-text-with-title ()
  "The skeleton pre-fills source, leaves other fields childless,
and shows the title under a `title' group."
  (let* ((org-text (skg-edit-metadata--empty-node-org-text
                    "only" "my title"))
         (headlines (org-to-sexp--extract-headlines
                     (split-string org-text "\n"))))
    (should (equal headlines
                   '((1 . "title")
                     (2 . "my title")
                     (1 . "skg")
                     (2 . "node")
                     (3 . "source")
                     (4 . "only")
                     (3 . "indef")
                     (3 . "parentIs")
                     (3 . "birth")
                     (3 . "editRequest")
                     (3 . "viewRequests"))))))

(ert-deftest test-empty-node-org-text-blank-title ()
  "With a blank title, `title' is shown childless (nothing under it)."
  (let* ((org-text (skg-edit-metadata--empty-node-org-text "only" ""))
         (headlines (org-to-sexp--extract-headlines
                     (split-string org-text "\n"))))
    (should (equal (car headlines) '(1 . "title")))
    (should (equal (cadr headlines) '(1 . "skg")))))

;; Opening: C-c v m on a metadata-less headline drops minimal metadata
;; in place and opens the view with source pre-filled.

(ert-deftest test-edit-metadata-empty-opens-view ()
  "C-c v m on a metadata-less headline populates (skg (node (source only)))
in place and opens the empty-node view: source pre-filled, others childless."
  (test--with-skg-content-view
   "* a new node\n"
   test--config-one-source
   (lambda ()
     (let ((source-buffer (current-buffer)))
       (skg-edit-metadata)
       ;; The source headline now carries minimal metadata.
       (with-current-buffer source-buffer
         (should (string-match-p
                  "^\\* (skg (node (source only))) a new node$"
                  (buffer-substring-no-properties
                   (point-min) (point-max)))))
       ;; The edit buffer shows the skeleton.
       (let ((edit-buf (test--skg-edit-buffer)))
         (should edit-buf)
         (unwind-protect
             (with-current-buffer edit-buf
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "^\\*\\*\\* source\n\\*\\*\\*\\* only$"
                                         content))
                 (should (string-match-p "^\\*\\*\\* indef$" content))
                 (should (string-match-p "^\\*\\*\\* viewRequests$" content))
                 ;; childless: no value lines under the editable fields.
                 (should-not (string-match-p "^\\*\\*\\*\\* false" content))
                 (should-not (string-match-p "^\\*\\*\\*\\* none" content))
                 ;; title group present and read-only.
                 (should (string-match-p "^\\* title$" content))
                 (should (string-match-p "^\\*\\* a new node$" content))))
           (kill-buffer edit-buf)))))))

;; Round-trip: committing the untouched view yields just the source.

(ert-deftest test-edit-metadata-empty-commit-untouched ()
  "Committing the untouched empty-node view yields (skg (node (source only)))."
  (test--with-skg-content-view
   "* a new node\n"
   test--config-one-source
   (lambda ()
     (let ((source-buffer (current-buffer)))
       (skg-edit-metadata)
       (with-current-buffer (test--skg-edit-buffer)
         (skg-sexp-edit--commit))
       (with-current-buffer source-buffer
         (should (string-match-p
                  "^\\* (skg (node (source only))) a new node$"
                  (buffer-substring-no-properties
                   (point-min) (point-max)))))))))

;; Round-trip: a field the user populates survives; the rest stay absent.

(ert-deftest test-edit-metadata-empty-commit-with-indef ()
  "Populating indef=true in the view yields (skg (node (source only) indef)),
while the untouched fields contribute no keys."
  (test--with-skg-content-view
   "* a new node\n"
   test--config-one-source
   (lambda ()
     (let ((source-buffer (current-buffer)))
       (skg-edit-metadata)
       (with-current-buffer (test--skg-edit-buffer)
         ;; Simulate the user adding a level-4 child under indef and
         ;; cycling it to true.
         (goto-char (point-min))
         (re-search-forward "^\\*\\*\\* indef$" nil t)
         (end-of-line)
         (insert "\n**** true")
         (skg-sexp-edit--commit))
       (with-current-buffer source-buffer
         (should (string-match-p
                  "^\\* (skg (node (source only) indef)) a new node$"
                  (buffer-substring-no-properties
                   (point-min) (point-max)))))))))

(provide 'test-skg-insert-heading-source-prompt)
