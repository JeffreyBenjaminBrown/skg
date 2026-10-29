;;; test-skg-detach-attach.el --- skg-detach and skg-attach  -*- lexical-binding: t; -*-
;;;
;;; Under a parent that reads claims, skg-detach (C-c s t) writes
;;; 'noClaim' on the headline at point and skg-attach (C-c s a)
;;; removes it. Both refuse under a parent that does not read claims,
;;; say so when there is nothing to do, and never save.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-metadata)
(require 'skg-keymaps-and-aliases)

(defun test-skg-detach-attach--run (text line command)
  "Insert TEXT in an org buffer, go to its LINE (1-based), and call
COMMAND. Returns (MESSAGE . BUFFER-TEXT), MESSAGE the one COMMAND
showed, or (error . MESSAGE) when it signaled a `user-error'."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((shown nil))
      (condition-case err
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq shown (apply #'format format-string args)))))
            (funcall command)
            (cons shown (buffer-string)))
        (user-error (cons 'error (cadr err)))))))

(defconst test-skg-detach-attach--editable-parent
  "* (skg node (gnid p) (skgRepos (nodeRepo r))) p\n")

(ert-deftest test-skg-detach-writes-noClaim ()
  (let ((result (test-skg-detach-attach--run
                 (concat test-skg-detach-attach--editable-parent
                         "** (skg node (gnid c) (skgRepos (nodeRepo r)) (view (rels (contains (in 1))))) c\n")
                 2 #'skg-detach)))
    (should (string-prefix-p "Detached" (car result)))
    (should (string-match-p
             "^\\*\\* (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim (view (rels (contains (in 1))))) c$"
             (cdr result)))))

(ert-deftest test-skg-detach-writes-noClaim-at-its-rank ()
  "'noClaim' lands where the server writes it: after the skgrepos and
before '(editRequest ...)' and '(viewRequests ...)'."
  (let ((result (test-skg-detach-attach--run
                 (concat test-skg-detach-attach--editable-parent
                         "** (skg node (gnid c) (skgRepos (nodeRepo r)) (editRequest (merge m)) (viewRequests editableView) (view (rels (contains (in 1))))) c\n")
                 2 #'skg-detach)))
    (should (string-prefix-p "Detached" (car result)))
    (should (string-match-p
             (regexp-quote "** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim (editRequest (merge m)) (viewRequests editableView) (view (rels (contains (in 1))))) c")
             (cdr result)))))

(ert-deftest test-skg-attach-removes-noClaim ()
  (let ((result (test-skg-detach-attach--run
                 (concat test-skg-detach-attach--editable-parent
                         "** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) c\n")
                 2 #'skg-attach)))
    (should (string-prefix-p "Attached" (car result)))
    (should (string-match-p "^\\*\\* (skg node (gnid c) (skgRepos (nodeRepo r))) c$"
                            (cdr result)))))

(ert-deftest test-skg-detach-a-headline-without-metadata ()
  "A headline the user typed is a node claiming membership; detaching
it gives it the node's atom and 'noClaim'."
  (let ((result (test-skg-detach-attach--run
                 (concat test-skg-detach-attach--editable-parent
                         "** a typed child\n")
                 2 #'skg-detach)))
    (should (string-prefix-p "Detached" (car result)))
    (should (string-match-p "^\\*\\* (skg node noClaim) a typed child$"
                            (cdr result)))))

(ert-deftest test-skg-detach-and-attach-under-an-editable-folder ()
  "An editable partner folder under an editable node reads claims."
  (let ((text (concat test-skg-detach-attach--editable-parent
                      "** (skg subscribeeFolder)\n"
                      "*** (skg node (gnid s) (skgRepos (nodeRepo r))) s\n")))
    (should (string-match-p
             "^\\*\\*\\* (skg node (gnid s) (skgRepos (nodeRepo r)) noClaim) s$"
             (cdr (test-skg-detach-attach--run text 3 #'skg-detach))))))

(ert-deftest test-skg-detach-and-attach-refuse-where-no-claim-is-read ()
  "Under a write-protected parent, a write-protected folder, or no
parent at all, both refuse and change nothing."
  (dolist (case
           (list
            (cons (concat "* (skg node (gnid p) (skgRepos (nodeRepo r)) writeProtected) p\n"
                          "** (skg node (gnid c) (skgRepos (nodeRepo r))) c\n")
                  2)
            (cons (concat test-skg-detach-attach--editable-parent
                          "** (skg subscriberFolder)\n"
                          "*** (skg node (gnid s) (skgRepos (nodeRepo r)) noClaim) s\n")
                  3)
            (cons test-skg-detach-attach--editable-parent 1)))
    (dolist (command (list #'skg-detach #'skg-attach))
      (let ((result (test-skg-detach-attach--run
                     (car case) (cdr case) command)))
        (should (eq (car result) 'error))
        (should (string-match-p "does not read claims" (cdr result)))))))

(ert-deftest test-skg-detach-and-attach-say-so-on-a-no-op ()
  (dolist (case
           (list (list "** (skg node (gnid c) (skgRepos (nodeRepo r))) c\n"
                       #'skg-attach "nothing to attach")
                 (list "** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) c\n"
                       #'skg-detach "nothing to detach")))
    (let* ((text (concat test-skg-detach-attach--editable-parent (nth 0 case)))
           (result (test-skg-detach-attach--run text 2 (nth 1 case))))
      (should (string-match-p (nth 2 case) (car result)))
      (should (equal (cdr result) text)))))

(ert-deftest test-skg-detach-refuses-a-kind-that-claims-nothing ()
  (let ((result (test-skg-detach-attach--run
                 (concat test-skg-detach-attach--editable-parent
                         "** (skg aliasFolder)\n")
                 2 #'skg-detach)))
    (should (eq (car result) 'error))
    (should (string-match-p "claims no membership" (cdr result)))))

(ert-deftest test-skg-detach-and-attach-keys ()
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s t"))
              #'skg-detach))
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s a"))
              #'skg-attach)))

(provide 'test-skg-detach-attach)
