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
           :filter #'skg-doc-display-response
           :coding 'utf-8
           :nowait nil)))
  skg-doc--proc)

(defun skg-doc-display-response (proc string)
  "Display an s-expression from the Rust server."
  (with-current-buffer
      (get-buffer-create skg-doc-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert string))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer))))

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
