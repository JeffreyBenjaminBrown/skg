;; USAGE:
;; First evaluate the whole buffer (`M-x eval-buffer`).
;; Then evaluate the following commands individually.
;;
;; (skg-doc-connect)
;; (request-document-from-node "a")
;; (skg-doc-disconnect)
;;
;; The second of those asks Rust to ask TypeDB for
;; the document containing the node with the specified ID,
;; builds an s-expression representing an org-document
;; based on the result, and sends that to Emacs,
;; causing Emacs to open an org-mode buffer
;; displaying the results.

(defvar skg-doc--proc nil
  "Persistent TCP connection to the Rust backend.")

(defvar skg-doc-buffer-name "*skg-document*"
  "Buffer name for displaying s-expressions from the Rust server.")

(defun skg-doc-connect ()
  "Connect, persistently, to the Rust TCP server."
  (unless (and skg-doc--proc
               (process-live-p skg-doc--proc))
    (setq skg-doc--proc
          (make-network-process
           :name "skg-doc"
           :buffer "*skg-doc-raw*"
           :host "127.0.0.1"
           :service 1730
           :filter ;; handles the response
           #'skg-open-org-buffer-from-rust-s-exp
           :coding 'utf-8
           :nowait nil)))
  skg-doc--proc)

(defun skg-open-org-buffer-from-rust-s-exp (proc string)
  "Interpret the s-expression from the Rust server and display as org-mode."
  (with-current-buffer
      (get-buffer-create skg-doc-buffer-name)
    (let ((inhibit-read-only t)
          (s-expr (car (read-from-string string))))
      (erase-buffer)
      (org-mode)
      (let ((content (cdr (assq 'content s-expr))))
        ;; Ignore `view` key, focus on `content` key.
        (skg-doc-insert-node content 1)))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer))))

(defun skg-doc-get-property (node property-key)
  "Get PROPERTY from NODE."
  (message "skg-doc-get-property going.")
  (when (and node (consp node))
    (if (and (symbolp (car node))
             (eq (car node) property-key))
        (cdr node)
      (cdr (assq property-key node)))))

(defun skg-doc-insert-node (node level)
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

(defun request-document-from-node (node-id)
  "Request a document (as an s-expression)."
  (interactive "sNode ID: ")
  (let* ((proc (skg-doc-connect))
         (request-sexp
          (format "((request . \"single document\") (id . \"%s\"))\n" node-id)))
    (process-send-string proc request-sexp)))

(defun skg-doc-disconnect ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-doc--proc)
    (delete-process skg-doc--proc)
    (setq skg-doc--proc nil)))
