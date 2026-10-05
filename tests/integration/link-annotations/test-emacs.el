;;; -*- lexical-binding: t; -*-
;;; Live graph lookup, display overlays, and graph publication in Emacs.

(load-file "../../../elisp/skg-init.el")

(setq skg-port (string-to-number (getenv "SKG_TEST_PORT")))
(setq skg-config-dir (file-name-as-directory (getenv "SKG_TEST_DATA_DIR")))
(skg-tcp-connect-to-rust)
(unless (skg-herald-rules-ensure)
  (error "No herald rules from integration server"))

(defun link-test-wait (predicate)
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (funcall predicate)
      (error "Timed out waiting for link annotation response"))))

(defun link-test-status (id kind)
  (eq (car (gethash id skg-link-annotations--cache)) kind))

(defun link-test-suffix (fragment)
  (cl-some
   (lambda (overlay)
     (let ((text (overlay-get overlay 'after-string)))
       (and text (string-match-p (regexp-quote fragment) text))))
   (overlays-in (point-min) (point-max))))

(defvar link-test-broken-before 0
  "How many links were styled as broken before deleting a target.")

(defun link-test-broken-count ()
  "How many link labels are styled as broken links."
  (cl-count-if
   (lambda (overlay)
     (let ((face (overlay-get overlay 'face)))
       (and (consp face) (plist-get face :underline))))
   (overlays-in (point-min) (point-max))))

(let* ((data (getenv "SKG_TEST_DATA_DIR"))
       (view-text
        (concat "* (skg (node (id src) (repo public))) "
                "Repo [[id:old-dest][café]]\n"
                "[[id:gone][gone]] [[id:private-node][private]]\n"))
       (buffer (skg-open-org-buffer-from-text
                nil view-text "*skg-link-integration*" "link-integration")))
  (with-current-buffer buffer
    (link-test-wait (lambda ()
                      (and (link-test-status "old-dest" 'resolved)
                           (link-test-status "gone" 'missing)
                           (link-test-status "private-node" 'inactive))))
    (unless (equal (buffer-string) view-text)
      (error "Annotation changed buffer text"))
    (unless (and (not (buffer-modified-p))
                 (not skg-link-annotations--repo-suffix-enabled))
      (error "Initial annotations changed state"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (skg-limit-repo-set "all"))
    (link-test-wait
     (lambda () (and (link-test-status "private-node" 'resolved)
                     (= skg-lp--pending-count 0))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (skg-limit-repo-set "public"))
    (link-test-wait
     (lambda () (and (link-test-status "private-node" 'inactive)
                     (= skg-lp--pending-count 0))))
    (skg-toggle-repo-overlay-on-links)
    (unless (and (link-test-suffix "⌂:PUB")
                 (link-test-suffix "⌂:inactive")
                 (not (link-test-suffix "PRIV"))
                 (not (link-test-suffix "missing"))
                 (> (link-test-broken-count) 0))
      (error "Repo suffixes did not distinguish visible and unavailable links"))
    (org-fold-hide-subtree)
    (org-fold-show-all)
    (unless (equal (buffer-string) view-text)
      (error "Folding changed annotated text"))
    (skg-replace-buffer-with-new-content nil view-text)
    (unless (and skg-link-annotations--repo-suffix-enabled
                 (not (buffer-modified-p)))
      (error "Repo toggle or clean state lost across view replacement"))
    (goto-char (point-max))
    (insert "[[id:new][new]]\n")
    (skg-link-annotations-refresh)
    (link-test-wait (lambda () (link-test-status "new" 'missing)))
    (let ((unsaved-text (buffer-string)))
      (with-temp-file (expand-file-name "public/new.skg" data)
        (insert "pid: new\ntitle: New target\n"))
      (skg-rebuild-ephemeral-data-stores)
      (link-test-wait (lambda () (link-test-status "new" 'resolved)))
      (unless (and (equal (buffer-string) unsaved-text)
                   (buffer-modified-p))
        (error "Graph refresh altered unsaved link edits")))
    (rename-file (expand-file-name "private/private-node.skg" data)
                 (expand-file-name "public/private-node.skg" data))
    (skg-rebuild-ephemeral-data-stores)
    (link-test-wait (lambda ()
                      (link-test-status "private-node" 'resolved)))
    (setq link-test-broken-before (link-test-broken-count))
    (delete-file (expand-file-name "public/dest.skg" data))
    (skg-rebuild-ephemeral-data-stores)
    (link-test-wait (lambda () (link-test-status "old-dest" 'missing)))
    (link-test-wait (lambda () (> (link-test-broken-count) link-test-broken-before)))
    (unless (> (link-test-broken-count) link-test-broken-before)
      (error "A link to the deleted target was not styled as broken")))
  (message "PASS: live Emacs link annotation cycle")
  (kill-emacs 0))
