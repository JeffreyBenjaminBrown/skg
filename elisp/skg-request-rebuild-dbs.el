;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'skg-buffer-registry)
(require 'skg-maintenance)

(defun skg--rebuild-dirty-buffers ()
  (cl-remove-if-not #'skg-buffer-dirty-p (skg-registered-buffers)))

(defun skg--rebuild-raw-buffer-p (buffer)
  (with-current-buffer buffer
    (and skg--buffer-record
         (eq (skg--buffer-record-kind skg--buffer-record) 'raw-skg-file))))

(defun skg--rebuild-terminal (_response)
  (ding)
  (message "Skg databases rebuilt and registered views reconciled"))

(defun skg-rebuild-dbs ()
  "Archive editor state and rebuild all selected stores from exact disk bytes."
  (interactive)
  (when (and skg--maintenance-client-incident
             (not (eq skg--graph-write-admission 'open)))
    (user-error "Maintenance is already active"))
  (let* ((dirty (skg--rebuild-dirty-buffers))
         (dirty-raw (cl-remove-if-not #'skg--rebuild-raw-buffer-p dirty)))
    (when dirty-raw
      (user-error "Full rebuild refuses modified raw .skg buffers: %s"
                  (mapconcat #'buffer-name dirty-raw ", ")))
    (when (or (null dirty)
              (yes-or-no-p
               (format
                (concat "Full rebuild will archive %d dirty Skg view(s). "
                        "Impacted views may become detached recovery buffers. "
                        "Continue? ")
                (length dirty))))
      (message "Preparing recovery archive for full rebuild ...")
      (skg-begin-maintenance
       "full-rebuild" nil nil nil #'skg--rebuild-terminal))))

(provide 'skg-request-rebuild-dbs)
