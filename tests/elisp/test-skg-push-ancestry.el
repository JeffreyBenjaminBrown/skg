;;; test-skg-push-ancestry.el --- Tests for pushing an ancestry to the linkstack -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-push-ancestry)

(defconst test-skg-push-ancestry-buffer
  (concat
   "* (skg node (gnid a)) a\n"
   "** (skg node (gnid b) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b\n"
   "*** (skg forksFolder)\n"
   "**** (skg node (gnid c) (view (rels (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) c\n"
   "***** (skg node (gnid d) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) d\n")
  "Jeff's example: a contains b, whose forks folder holds c, which b
forks; c contains d.")

(defconst test-skg-push-ancestry-tree
  '("a" "a" nil (("b" "b" (contains in) (("c" "c" (forks in)))))))

(defun test-skg-push-ancestry--from (buffer-text search)
  "Push the ancestry of the headline of BUFFER-TEXT holding SEARCH."
  (with-temp-buffer
    (org-mode)
    (insert buffer-text)
    (goto-char (point-min))
    (search-forward search)
    (skg-push-ancestry)))

(ert-deftest test-skg-push-ancestry-of-jeffs-example ()
  "Pushing from c pushes a / b ~ c, the forks folder skipped."
  (let (( skg-linkstack '(("old" "old")) ))
    (test-skg-push-ancestry--from test-skg-push-ancestry-buffer "gnid c")
    (should (equal skg-linkstack
                   (list test-skg-push-ancestry-tree '("old" "old"))))))

(ert-deftest test-skg-push-ancestry-from-a-root-pushes-one-note ()
  (let (( skg-linkstack nil ))
    (test-skg-push-ancestry--from test-skg-push-ancestry-buffer "gnid a")
    (should (equal skg-linkstack '(("a" "a"))))))

(ert-deftest test-skg-push-ancestry-refuses-a-step-no-edge-joins ()
  "A note no edge joins to the note above it (an indirect mirror, here)
refuses the push, naming both notes, and pushes nothing."
  (let (( skg-linkstack nil ))
    (let (( err (should-error
                 (test-skg-push-ancestry--from
                  (concat "* (skg node (gnid a)) a\n"
                          "** (skg forksFolder)\n"
                          "*** (skg node (gnid m) (view (rels (forks (in 1))))) m\n")
                  "gnid m")
                 :type 'user-error) ))
      (should (string-match-p "\"m\" to \"a\"" (cadr err))))
    (should (null skg-linkstack))))

(ert-deftest test-skg-push-ancestry-refuses-a-folder ()
  (let (( skg-linkstack nil ))
    (should-error (test-skg-push-ancestry--from test-skg-push-ancestry-buffer
                                                "forksFolder")
                  :type 'user-error)
    (should (null skg-linkstack))))

(ert-deftest test-skg-ancestry-edge-each-kind-and-direction ()
  "Each relation, each way, read from the sides' ancestors."
  (dolist (case '( ((contains (in 1 (ancestors 1)))  1 (contains in))
                   ((contains (out 1 (ancestors 2))) 2 (contains out))
                   ((linksTo (in 3 (ancestors 1) (substantive 1 (ancestors 1)))) 1 (linksTo in))
                   ((linksTo (out 1 (ancestors 2))) 2 (linksTo out))
                   ((forks (in 1 (ancestors 2)))     2 (forks in))
                   ((forks (out 1 (ancestors 2)))    2 (forks out))
                   ((forks (in 1 (ancestors 2)) (out 1 (ancestors 2))) 2 (forks in out))
                   ((hides (in 1 (ancestors 3)))     3 (hides in))
                   ((hides (out 1 (ancestors 2)))    2 (hides out))
                   ((contains (in 1 (ancestors 4)))  2 nil) ))
    (should (equal (skg--ancestry-edge
                    `(skg node (gnid x) (view (rels ,(car case))))
                    (nth 1 case))
                   (nth 2 case)))))

(ert-deftest test-skg-ancestry-edge-prefers-the-birth-fact ()
  "A member of a hidden-in-fork-partner folder: the partner two up
contains it, the note four up hides it; at generation 2 the birth
fact chooses contains over the relation order."
  (let (( sexp '(skg node (gnid x)
                     (view (rels (contains (in 1 (ancestors 2)))
                                 (hides (in 1 (ancestors 4))))
                           (birth (hides in 4) (contains in 2)))) ))
    (should (equal (skg--ancestry-edge sexp 2) '(contains in)))
    (should (equal (skg--ancestry-edge sexp 4) '(hides in))))
  (should (equal (skg--ancestry-edge
                  '(skg node (gnid x)
                        (view (rels (contains (in 1 (ancestors 1)))
                                    (linksTo (in 1 (ancestors 1))))
                              (birth (linksTo in 1))))
                  1)
                 '(linksTo in))))

(ert-deftest test-skg-linkstack-edge-text-and-herald ()
  (dolist (case '( ((contains in)     "/"            "aC")
                   ((contains out)    "\\"           "Ca")
                   ((linksTo in)      "->"           "aL")
                   ((linksTo out)     "<-"           "La")
                   ((forks in)        "~"            "aK")
                   ((forks out)       "~"            "Ka")
                   ((forks in out)    "~"            "aKa")
                   ((hides in)        "hides"        "aH")
                   ((hides out)       "is hidden by" "Ha") ))
    (should (equal (skg--linkstack-edge-text (car case)) (nth 1 case)))
    (should (equal (skg--linkstack-edge-herald (car case)) (nth 2 case)))
    (should (equal (skg--linkstack-edge-from-herald (nth 2 case)) (car case)))))

(ert-deftest test-skg-linkstack-tree-text-is-a-chain-of-links ()
  (should (equal (skg--linkstack-tree-text test-skg-push-ancestry-tree "c, edited")
                 "[[id:a][a]] / [[id:b][b]] ~ [[id:c][c, edited]]"))
  (should (equal (skg--linkstack-tree-text '("x" "x") "y")
                 "[[id:x][y]]"))
  (should (equal (skg--linkstack-tree-prefix-for-prompt test-skg-push-ancestry-tree)
                 "a / b ~ ")))

(ert-deftest test-skg-paste-and-pop-link-of-a-chain ()
  "Pasting a chain asks only the leaf's label, starting from its own;
popping removes the tree pasted. ID and node pastes use the leaf."
  (let (( skg-linkstack (list test-skg-push-ancestry-tree '("x" "x")) )
        ( asked nil ))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt initial &rest _)
                 (push (list prompt initial) asked)
                 "c2")))
      (with-temp-buffer
        (skg-paste-link)
        (should (equal (buffer-string)
                       "[[id:a][a]] / [[id:b][b]] ~ [[id:c][c2]]"))
        (should (equal asked
                       '(("Link label (1/2; S-left/right: another tree): a / b ~ " "c"))))
        (should (= (length skg-linkstack) 2))
        (erase-buffer)
        (skg-pop-link)
        (should (equal skg-linkstack '(("x" "x"))))))
    (with-temp-buffer
      (let (( skg-linkstack (list test-skg-push-ancestry-tree) ))
        (skg-paste-id)
        (should (equal (buffer-string) "c"))))))

(ert-deftest test-skg-paste-link-cycles-the-trees ()
  "S-right and S-left in the label prompt show another tree, cycling;
the tree pasted, and popped, is the one shown when the label is given."
  (let* (( skg-linkstack (list '("x" "x") test-skg-push-ancestry-tree '("y" "y")) )
         ( keys '(1 1 1 -1) ) ;; to the 2nd, the 3rd, round to the 1st, back to the 3rd
         ( prompts nil ))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt initial &rest _)
                 (push (cons prompt initial) prompts)
                 (if keys
                     (throw 'skg--linkstack-cycle (pop keys))
                   initial))))
      (with-temp-buffer
        (skg-pop-link)
        (should (equal (buffer-string) "[[id:y][y]]"))))
    (should (equal (mapcar #'cdr (reverse prompts)) '("x" "c" "y" "x" "y")))
    (should (string-match-p "(2/3; .*: a / b ~ \\'" (car (nth 3 prompts))))
    (should (equal skg-linkstack (list '("x" "x") test-skg-push-ancestry-tree)))))

(ert-deftest test-skg-linkstack-cycle-keys-throw ()
  "The label prompt's S-left and S-right throw to the cycle tag."
  (let (( skg-linkstack '(("x" "x")) )
        ( thrown nil ))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _)
                 (with-temp-buffer
                   (use-local-map (make-sparse-keymap))
                   (run-hooks 'minibuffer-setup-hook)
                   (setq thrown
                         (list (catch 'skg--linkstack-cycle
                                 (call-interactively (key-binding (kbd "S-<left>"))))
                               (catch 'skg--linkstack-cycle
                                 (call-interactively (key-binding (kbd "S-<right>")))))))
                 "x")))
      (skg--linkstack-read-leaf-label 0))
    (should (equal thrown '(-1 1)))))

(ert-deftest test-skg-linkstack-view-round-trips-trees ()
  "The stack view draws a tree as nested headlines, each child's title
led by its edge; saving it reads the same trees back."
  (let (( skg-linkstack (list test-skg-push-ancestry-tree '("x" "x")) ))
    (should (equal (skg--format-linkstack-as-org)
                   (concat "* a\na\n** (aC) b\nb\n*** (aK) c\nc\n* x\nx")))
    (with-temp-buffer
      (insert (skg--format-linkstack-as-org))
      (should (equal (skg-validate-linkstack-buffer)
                     (list 'success skg-linkstack))))))

(ert-deftest test-skg-linkstack-view-refuses-a-child-without-an-edge ()
  (dolist (text '("* a\na\n** b\nb\n"            ;; no edge
                  "* a\na\n** (xC) b\nb\n"       ;; no edge herald
                  "* a\na\n*** (aC) b\nb\n"      ;; two levels down
                  "** (aC) b\nb\n"))             ;; a child of nothing
    (with-temp-buffer
      (insert text)
      (should (eq (car (skg-validate-linkstack-buffer)) 'error)))))

(ert-deftest test-skg-id-push-is-an-obsolete-alias-of-skg-push-id ()
  (should (eq (indirect-function 'skg-id-push) (indirect-function 'skg-push-id)))
  (should (get 'skg-id-push 'byte-obsolete-info))
  (let (( skg-linkstack nil ))
    (with-temp-buffer
      (org-mode)
      (insert "* (skg node (gnid q)) q\n")
      (goto-char (point-min))
      (with-no-warnings (skg-id-push)))
    (should (equal skg-linkstack '(("q" "q"))))))

(ert-deftest test-skg-push-ancestry-key ()
  (with-temp-buffer
    (skg-content-view-mode)
    (should (eq (key-binding (kbd "C-c U")) 'skg-push-ancestry))
    (should (eq (key-binding (kbd "C-c u")) 'skg-push-id))))

(provide 'test-skg-push-ancestry)
