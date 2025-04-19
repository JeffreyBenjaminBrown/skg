;; USAGE
;; (skg-doc-connect)
;; (request-document-from-node "5")
;; (skg-doc-disconnect)

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
           :filter #'skg-doc-interpret-response
           :coding 'utf-8
           :nowait nil)))
  skg-doc--proc)

(defun skg-doc-interpret-response (proc string)
  "Interpret the s-expression from the Rust server and display as org-mode."
  (with-current-buffer
      (get-buffer-create skg-doc-buffer-name)
    (let ((inhibit-read-only t)
          (s-expr (car (read-from-string string))))
      (erase-buffer)
      (org-mode)
      (skg-doc-insert-org-from-sexpr s-expr))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer))))

(defun skg-doc-get-property (node property)
  "Get PROPERTY from NODE."
  (when (and node (consp node))
    (if (and (stringp (car node))
             (equal (car node) property))
        (cdr node)
      (cdr (assoc property node)))))

(defun skg-doc-insert-org-from-sexpr (s-expr)
  "Insert org-mode content from S-EXPR."
  (let ((content (cdr (assoc 'content s-expr))))
    ;; Check if content is a list of nodes or a single node with properties
    (if (and (consp content) (consp (car content)) (stringp (cdar content)))
        ;; It's a single node with properties
        (skg-doc-insert-node content 1)
      ;; It's a list of nodes
      (dolist (node content)
        (skg-doc-insert-node node 1)))))

(defun skg-doc-insert-node (node level)
  "Insert NODE at indentation LEVEL."
  (let*
      ((is-node-list (and (listp node)
                          (listp (car node))
                          (stringp (caar node))))
       ;; Extract properties based on whether this is a node list or node object
       (id
        (if is-node-list
            (cdr (assoc "id" node))
          (skg-doc-get-property node "id")))
       (headline
        (if is-node-list
            (cdr (assoc "headline" node))
          (skg-doc-get-property node "headline")))
       (unindexed-text
        (if is-node-list
            (cdr (assoc "unindexed_text" node))
          (skg-doc-get-property node "unindexed_text")))
       (focused
        (if is-node-list
            (cdr (assoc "focused" node))
          (skg-doc-get-property node "focused")))
       (content
        (if is-node-list
            (cdr (assoc "content" node))
          (skg-doc-get-property node "content"))))

    ;; Only proceed if we have a headline
    (when headline
      ;; Create the bullet (asterisks) with text properties for ID and focused
      (let ((bullets (make-string level ?*))
            (start (point)))
        (insert bullets " ")
        ;; Add text properties to the bullets
        (add-text-properties
         start (+ start level)
         (append (list 'id id)
                 (when focused (list 'focused t)))))
      (insert headline)
      (when unindexed-text
        (insert "\n" unindexed-text))
      (insert "\n")
      (when content
        (if (and (listp content)
                 (listp (car content))
                 (listp (caar content)))
            ;; List of nodes
            (dolist (child content)
              (skg-doc-insert-node child
                                   (1+ level)))
          ;; Single node (shouldn't happen)
          (when (listp content)
            (skg-doc-insert-node content
                                 (1+ level))))))))

(defun request-document-from-node (node-id)
  "Request a document (as an s-expression)."
  (interactive "sNode ID: ")
  (let ((proc (skg-doc-connect)))
    (process-send-string proc (concat node-id "\n"))))

(defun skg-doc-disconnect ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-doc--proc)
    (delete-process skg-doc--proc)
    (setq skg-doc--proc nil)))
