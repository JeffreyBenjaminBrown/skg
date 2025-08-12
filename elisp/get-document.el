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
  "Request a document (as an s-expression)."
  (interactive "sNode ID: ")
  (let* ((proc (skg-tcp-connect-to-rust))
         (request-sexp
          (format "((request . \"single document\") (id . \"%s\"))\n" node-id)))
    (setq skg-doc--response-handler
          ;; Prepare for response.
          #'skg-open-org-buffer-from-rust-s-exp)
    (process-send-string proc request-sexp)))

(defun skg-open-org-buffer-from-rust-s-exp (proc string)
  "Interpret the s-expression from the Rust server and display as org-mode."
  (with-current-buffer
      (get-buffer-create skg-content-buffer-name)
    (let ((inhibit-read-only t)
          (s-expr (car (read-from-string string))))
      (erase-buffer)
      (org-mode)
      (let ((content (cdr (assq 'content s-expr))))
        ;; Ignore `view` key, focus on `content` key.
        (skg-doc-insert-node content 1)))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer))))

(defun skg-doc-insert-node
    (node ;; see the DATA comment, top of this file
     level) ;; a positive integer
  "The recursive workhorse for building up the org document. It inserts NODE at indentation LEVEL."
  (let* ( (id (cdr (assq 'id node)))
          (heading (cdr (assq 'heading node)))
          (body
           (cdr (assq 'body node)))
          (focused (cdr (assq 'focused node)))
          (repeated (cdr (assq 'repeated node)))
          (content (cdr (assq 'content node))) )
    (let ;; insert bullet with text properties
        ((bullet (make-string level ?*))
         (start (point)))
      (insert bullet " ")
      (add-text-properties
       start (+ start level) ;; where the bullet is
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
