;; DATA USED/ASSUMED
;; In the following, a `node` is a particular kind of s-exp, specifically an association list with the following fields.
;;   'id → string
;;   'heading → string
;;   'body → string or absent.
;;   'focused → t or absent. See `rust/types.rs`.
;;   'folded → t or absent. See `rust/types.rs`.
;;   'repeated → t or absent. See `rust/types.rs`.
;;   'content → list of child nodes (recursive)

(defun request-document-from-node (node-id)
  "Sends a document request (as an s-expression).
Sets `skg-doc--response-handler` to prepare for the response.
Returns nil."
  (interactive "sNode ID: ")
  (let* ((proc (skg-tcp-connect-to-rust))
         (request-sexp
          (format "((request . \"single document\") (id . \"%s\"))\n"
                  node-id)) )
    (setq skg-doc--response-handler
          ;; Prepare for response.
          #'skg-open-org-buffer-from-rust-s-exp)
    (process-send-string proc request-sexp)) )

(defun skg-open-org-buffer-from-rust-s-exp
    ( _proc
      rust-sexp ) ;; string
  "Opens a new org-mode buffer.
Loads it with the content from the s-exp from Rust."
  (with-current-buffer
      (get-buffer-create "*skg-content-view*")
    (let ((inhibit-read-only t)
          (s-expr (car ;; In read-from-string's output, the CAR is the value read (and the "CDR is a number giving the position of the next remaining character in string").
                   (read-from-string rust-sexp)) ))
      (erase-buffer)
      (org-mode)
      (let ((content (cdr (assq 'content s-expr)) ))
        ;; Ignore `view` key, focus on `content` key. See `/api.md`.
        (skg-doc-insert-node content 1)))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer))))

(defun skg-doc-insert-node
    ( node    ;; See the DATA comment, top of this file.
      level ) ;; A positive integer. 1 = top-level heading.
  "PURPOSE: Recursively builds an org document,
by inserting NODE at indentation LEVEL, and then its children.
.
ASSUMES nothing on the same line is left of point."

  (let* ( (id (cdr (assq 'id node)))
          (heading (cdr (assq 'heading node)))
          (body
           (cdr (assq 'body node)))
          (focused (cdr (assq 'focused node)))
          ;; TODO: handle `folded`.
          (repeated (cdr (assq 'repeated node)))
          (content (cdr (assq 'content node))) )
    (let ;; insert bullet with text properties
        ((bullet (make-string level ?*))
         (start (point)) )
      (insert bullet " ")
      (add-text-properties
       start (+ start level) ;; the bullet's span
       (append (when focused (list 'focused t))
               (when repeated (list 'repeated t))
               (list 'id id)
               )))
    (insert heading "\n")
    (when body
      (insert body "\n"))
    (when content
      (dolist (child content)
        (skg-doc-insert-node child
                             (1+ level))))))

(defun skg-doc-get-property (node property-key)
  "Get PROPERTY from NODE."
  (message "skg-doc-get-property going.")
  (when (and node (consp node))
    (if (and (symbolp (car node))
             (eq (car node) property-key))
        (cdr node)
      (cdr (assq property-key node)))))

(provide 'get-document)
