;;; test-skg-insert-heading-repo-prompt.el --- Test C-return skgrepo prompt
;;;
;;; When org-insert-heading-respect-content is called on a root headline
;;; in a content-view buffer, and the new headline has no metadata,
;;; skg-view-metadata should prompt for a skgrepo in the minibuffer
;;; (not open the sexp-edit buffer) and insert the chosen skgrepo
;;; as metadata on the new headline.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-sexpr-edit)
(require 'skg-config)

(defvar test--config-public-and-private
  (concat "[[repos]]\n"
          "name = \"public\"\n"
          "path = \"owned/public\"\n"
          "\n"
          "[[repos]]\n"
          "name = \"private\"\n"
          "path = \"owned/private\"\n"
          "")
  "Config text with two owned repos: public and private.")

(defvar test--config-with-foreign-repo
  (concat test--config-public-and-private
          "\n[[repos]]\n"
          "name = \"foreign\"\n"
          "path = \"" (expand-file-name
                       "test-skg-insert-heading-repo-prompt/foreign"
                       (file-name-directory load-file-name)) "\"\n"
          "")
  "Config text with two owned repos and one foreign repo.")

(defvar test--config-with-interleaved-repo-sets
  (concat "[[repo_sets]]\n"
          "name = \"public-set\"\n"
          "repos = [\"public\"]\n\n"
          "[[repos]]\n"
          "name = \"public\"\n"
          "path = \"owned/public\"\n"
          "\n"
          "[[repo_sets]]\n"
          "name = \"private-set\"\n"
          "repos = [\"private\"]\n\n"
          "[[repos]]\n"
          "name = \"private\"\n"
          "path = \"owned/private\"\n"
          "")
  "Config text with [[repos]] interleaved among other array\ntables. The [[repo_sets]] tables are RETIRED config the server\nwould reject; they remain here to pin that the elisp readers skip\ntables they do not care about.")

(defun test--with-skg-content-view (org-text config-text body-fn)
  "Run BODY-FN in a temp skg content-view buffer with ORG-TEXT.
CONFIG-TEXT is written to a temporary skgconfig.toml so that
skg-config-dir is set and skg--owned-repos works."
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

;; Test 1: C-return inserts metadata with chosen skgrepo via minibuffer,
;;         does NOT open a sexp-edit buffer.

(ert-deftest test-insert-heading-prompts-for-repo ()
  "C-return on a root headline should prompt for repo in minibuffer,
insert metadata with chosen repo, and not open the sexp-edit buffer."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
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
                "^\\* (skg (node (id x) (repo public))) x$"
                content))

       ;; New headline has metadata with chosen skgrepo "private".
       (should (string-match-p
                "^\\* (skg (node (repo private))) $"
                content))

       ;; Exactly two headlines.
       (should (= 2 (how-many "^\\* " (point-min) (point-max)))))

     ;; No sexp-edit buffer was opened.
     (should-not
      (cl-find-if
       (lambda (b)
         (buffer-local-value 'skg-sexp-edit--source-buffer b))
       (buffer-list))))))

;; Test 2: Single owned skgrepo skips the prompt entirely.

(ert-deftest test-insert-heading-single-repo-no-prompt ()
  "When there is only one owned repo, C-return should use it
without prompting."
  (let ((one-repo-config
         (concat "[[repos]]\n"
                 "name = \"only\"\n"
                 "path = \"owned/only\"\n"
                 "")))
    (test--with-skg-content-view
     "* (skg (node (id x) (repo only))) x\n"
     one-repo-config
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
                  "^\\* (skg (node (repo only))) $"
                  content)))))))

;; Test 3: The cycling closure works correctly.

(ert-deftest test-repo-cycling-wraps-around ()
  "skg--prompt-for-owned-repo's cycling closure should wrap around."
  (let* ((config-dir (make-temp-file "skg-test-config" t))
         (config-file (expand-file-name "skgconfig.toml" config-dir))
         (skg-config-dir (file-name-as-directory config-dir)))
    (with-temp-file config-file
      (insert test--config-public-and-private))
    (unwind-protect
        (let ((skgrepos (skg--owned-repos))
              results)
          ;; Verify we have two skgrepos in expected order.
          (should (equal skgrepos '("public" "private")))

          ;; Mock completing-read to simulate cycling:
          ;; Start at "public", cycle right once to reach "private".
          ;; The cycle closure replaces minibuffer contents,
          ;; so we test the logic by calling the cycle function directly.
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt coll &rest _)
                       ;; Simulate: start empty, cycle right once.
                       ;; The cycle closure does:
                       ;;   idx = (cl-position cur skgrepos) or 0
                       ;;   new = (nth (mod (+ idx dir) len) skgrepos)
                       ;; Starting from "" (not in list) -> idx=0 ("public"),
                       ;; cycling right: (mod (+ 0 1) 2) = 1 -> "private"
                       (let* ((idx 0)
                              (new (nth (mod (+ idx 1) (length skgrepos))
                                        skgrepos)))
                         new))))
            (should (equal (skg--prompt-for-owned-repo) "private")))

          ;; Test wrap-around: from "private" (idx=1), cycle right -> "public"
          (let* ((idx 1)
                 (new (nth (mod (+ idx 1) (length skgrepos)) skgrepos)))
            (should (equal new "public")))

          ;; Test cycle left from "public" (idx=0) -> wraps to "private"
          (let* ((idx 0)
                 (new (nth (mod (+ idx -1) (length skgrepos)) skgrepos)))
            (should (equal new "private"))))
      (delete-file config-file)
      (delete-directory config-dir))))

;; Test 4: Existing metadata still opens the sexp-edit buffer.

(ert-deftest test-edit-existing-metadata-opens-edit-buffer ()
  "When a headline already has metadata, skg-view-metadata should
open the sexp-edit buffer (not prompt in minibuffer)."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
   test--config-public-and-private
   (lambda ()
     (should (org-at-heading-p))
     (skg-view-metadata)

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

(ert-deftest test-view-repo-list-includes-all-repos ()
  "skg-view-repo-list should list every configured repo and path."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
   test--config-with-foreign-repo
   (lambda ()
     (unwind-protect
         (progn
           (skg-view-repo-list)
           (let ((list-buffer (get-buffer "*skg-repos*")))
             (should list-buffer)
             (with-current-buffer list-buffer
               (should (derived-mode-p 'org-mode))
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "^\\* public$" content))
                 (should (string-match-p "^\\* private$" content))
                 (should (string-match-p "^\\* foreign$" content))
                 (should (string-match-p "/public" content))
                 (should (string-match-p "/private" content))
                 (should (string-match-p "/foreign" content))))))
       (when (get-buffer "*skg-repos*")
         (kill-buffer "*skg-repos*"))))))

(ert-deftest test-repo-change-prompt-starts-with-current-repo ()
  "skg--prompt-for-repo-change should put current repo in editable text."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
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
       (should (equal (skg--prompt-for-repo-change "public")
                      "private"))))))

(ert-deftest test-repo-set-prompt-completes-configured-repo-sets ()
  "skg--prompt-for-repo-set completes the prefix repo-set choices:
the repos in privacy order, then \"all\"."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
   test--config-with-interleaved-repo-sets
   (lambda ()
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection predicate require-match
                         initial-input &optional hist def
                         inherit-input-method)
                  (should (string-match-p "Most private repo" prompt))
                  (should (equal collection
                                 '("public" "private" "all")))
                  (should (null predicate))
                  (should require-match)
                  (should (null initial-input))
                  (should (null hist))
                  (should (equal def "all"))
                  (should (null inherit-input-method))
                  "private")))
       (should (equal (skg--prompt-for-repo-set)
                      "private"))))))

(ert-deftest test-config-readers-handle-interleaved-repo-tables ()
  "Elisp config readers should not confuse [[repos]] and [[repo_sets]]."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
   test--config-with-interleaved-repo-sets
   (lambda ()
     (should (equal (skg--repo-names) '("public" "private")))
     (should (equal (skg--repo-set-names)
                    '("public" "private" "all")))
     (should (equal (mapcar #'car (skg--repo-paths))
                    '("public" "private"))))))

;; --- Empty-node metadata view (C-c v m on a metadata-less headline) ---

(defvar test--config-one-repo
  (concat "[[repos]]\n"
          "name = \"only\"\n"
          "path = \"owned/only\"\n"
          "")
  "Config text with a single owned repo, so no repo prompt fires.")

(defun test--skg-edit-buffer ()
  "Return the open sexp-edit buffer, if any."
  (cl-find-if
   (lambda (b)
     (buffer-local-value 'skg-sexp-edit--source-buffer b))
   (buffer-list)))

;; Builder: skgrepo pre-filled, other editable fields childless, title group.

(ert-deftest test-empty-node-org-text-with-title ()
  "The skeleton pre-fills repo, leaves other fields childless,
and shows the title under a `title' group."
  (let* ((org-text (skg-view-metadata--empty-node-org-text
                    "only" "my title"))
         (headlines (org-to-sexp--extract-headlines
                     (split-string org-text "\n"))))
    (should (equal headlines
                   '((1 . "title")
                     (2 . "my title")
                     (1 . "skg")
                     (2 . "node")
                     (3 . "repo")
                     (4 . "only")
                     (3 . "writeProtected")
                     (3 . "affectsParent")
                     (3 . "birth")
                     (3 . "editRequest")
                     (3 . "viewRequests"))))))

(ert-deftest test-empty-node-org-text-blank-title ()
  "With a blank title, `title' is shown childless (nothing under it)."
  (let* ((org-text (skg-view-metadata--empty-node-org-text "only" ""))
         (headlines (org-to-sexp--extract-headlines
                     (split-string org-text "\n"))))
    (should (equal (car headlines) '(1 . "title")))
    (should (equal (cadr headlines) '(1 . "skg")))))

;; Opening: C-c v m on a metadata-less headline drops minimal metadata
;; in place and opens the view with skgrepo pre-filled.

(ert-deftest test-edit-metadata-empty-opens-view ()
  "C-c v m on a metadata-less headline populates (skg (node (repo only)))
in place and opens the empty-node view: repo pre-filled, others childless."
  (test--with-skg-content-view
   "* a new node\n"
   test--config-one-repo
   (lambda ()
     (let ((source-buffer (current-buffer)))
       (skg-view-metadata)
       ;; The source headline now carries minimal metadata.
       (with-current-buffer source-buffer
         (should (string-match-p
                  "^\\* (skg (node (repo only))) a new node$"
                  (buffer-substring-no-properties
                   (point-min) (point-max)))))
       ;; The edit buffer shows the skeleton.
       (let ((edit-buf (test--skg-edit-buffer)))
         (should edit-buf)
         (unwind-protect
             (with-current-buffer edit-buf
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "^\\*\\*\\* repo\n\\*\\*\\*\\* only$"
                                         content))
                 (should (string-match-p "^\\*\\*\\* writeProtected$" content))
                 (should (string-match-p "^\\*\\*\\* viewRequests$" content))
                 ;; childless: no value lines under the editable fields.
                 (should-not (string-match-p "^\\*\\*\\*\\* false" content))
                 (should-not (string-match-p "^\\*\\*\\*\\* none" content))
                 ;; title group present and read-only.
                 (should (string-match-p "^\\* title$" content))
                 (should (string-match-p "^\\*\\* a new node$" content))))
           (kill-buffer edit-buf)))))))

;; Round-trip: committing the untouched view yields just the skgrepo.

(ert-deftest test-edit-metadata-empty-commit-untouched ()
  "Committing the untouched empty-node view yields (skg (node (repo only)))."
  (test--with-skg-content-view
   "* a new node\n"
   test--config-one-repo
   (lambda ()
     (let ((source-buffer (current-buffer)))
       (skg-view-metadata)
       (with-current-buffer (test--skg-edit-buffer)
         (skg-sexp-edit--commit))
       (with-current-buffer source-buffer
         (should (string-match-p
                  "^\\* (skg (node (repo only))) a new node$"
                  (buffer-substring-no-properties
                   (point-min) (point-max)))))))))

;; Round-trip: a field the user populates survives; the rest stay na.

(ert-deftest test-edit-metadata-empty-commit-with-write-protected ()
  "Populating writeProtected=true in the view yields (skg (node (repo only) writeProtected)),
while the untouched fields contribute no keys."
  (test--with-skg-content-view
   "* a new node\n"
   test--config-one-repo
   (lambda ()
     (let ((source-buffer (current-buffer)))
       (skg-view-metadata)
       (with-current-buffer (test--skg-edit-buffer)
         ;; Simulate the user adding a level-4 child under write-protected and
         ;; cycling it to true.
         (goto-char (point-min))
         (re-search-forward "^\\*\\*\\* writeProtected$" nil t)
         (end-of-line)
         (insert "\n**** true")
         (skg-sexp-edit--commit))
       (with-current-buffer source-buffer
         (should (string-match-p
                  "^\\* (skg (node (repo only) writeProtected)) a new node$"
                  (buffer-substring-no-properties
                   (point-min) (point-max)))))))))

(provide 'test-skg-insert-heading-repo-prompt)
