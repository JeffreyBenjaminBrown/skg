;;; -*- lexical-binding: t; -*-
;;; Explicit lifecycle records for every Skg-owned editor buffer.

(require 'cl-lib)
(require 'org-id)
(require 'subr-x)
(require 'skg-state)

(cl-defstruct skg--buffer-record
  id kind lifecycle disposable continuation-id buffer view-uri recipe root-ids
  source-set graph-generation presentation-generation server-revision
  application-token last-fetched last-fetched-sha256 logical-dirty
  origin-buffer-id origin-token transient-lock-reasons maintenance-epoch
  presentation-stale search-stale herald-bearing)

(defvar skg--buffer-registry (make-hash-table :test #'equal)
  "Skg-owned buffers keyed by stable client-local buffer ID.")

(defvar-local skg--buffer-record nil)
(put 'skg--buffer-record 'permanent-local t)

(defvar-local skg--maintenance-lock-overlay nil)
(put 'skg--maintenance-lock-overlay 'permanent-local t)

(defun skg-buffer-raw-text (&optional buffer)
  "Return BUFFER's exact no-properties save-equivalent text."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun skg--utf8-unix-bytes (text)
  (encode-coding-string text 'utf-8-unix))

(defun skg--sha256-text (text)
  (secure-hash 'sha256 (skg--utf8-unix-bytes text)))

(defun skg--conservative-ids-from-text (text)
  "Collect recognizable metadata IDs from TEXT without validating the view."
  (let ((start 0) ids)
    (while (string-match
            "(\\(?:id\\|pid\\|extra_ids\\|extraIds\\) +\\([^()[:space:]]+\\)"
            text start)
      (push (match-string 1 text) ids)
      (setq start (match-end 0)))
    (delete-dups (nreverse ids))))

(cl-defun skg-register-buffer
    (buffer kind &key view-uri recipe root-ids lifecycle disposable
            continuation-id last-fetched server-revision graph-generation
            presentation-generation application-token)
  "Register BUFFER under an explicit KIND and return its durable record."
  (with-current-buffer buffer
    (let* ((existing skg--buffer-record)
           (id (or (and existing (skg--buffer-record-id existing))
                   (org-id-uuid)))
           (text (or last-fetched (skg-buffer-raw-text buffer)))
           (record (make-skg--buffer-record
                    :id id
                    :kind kind
                    :lifecycle (or lifecycle 'live-view)
                    :disposable disposable
                    :continuation-id continuation-id
                    :buffer buffer
                    :view-uri view-uri
                    :recipe recipe
                    :root-ids (or root-ids
                                  (skg--conservative-ids-from-text text))
                    :source-set skg--active-source-set-name
                    :graph-generation
                    (or graph-generation
                        (cdr (assq 'graph-generation skg--server-store-state)))
                    :presentation-generation
                    (or presentation-generation 0)
                    :server-revision (or server-revision 0)
                    :application-token
                    (or application-token
                        (if existing
                            (1+ (or (skg--buffer-record-application-token existing)
                                    0))
                          1))
                    :last-fetched text
                    :last-fetched-sha256 (skg--sha256-text text)
                    :logical-dirty nil
                    :transient-lock-reasons nil
                    :maintenance-epoch nil
                    :presentation-stale nil
                    :search-stale nil
                    :herald-bearing (string-match-p "(heralds\\_>" text))))
      (when existing
        (remhash (skg--buffer-record-id existing) skg--buffer-registry))
      (setq skg--buffer-record record)
      (puthash id buffer skg--buffer-registry)
      (add-hook 'kill-buffer-hook #'skg-unregister-current-buffer nil t)
      (setq-local mode-line-process
                  '(:eval (skg-buffer-status-indicator)))
      (when (and (listp skg--maintenance-state)
                 (equal (format "%s"
                                (cdr (assq 'state skg--maintenance-state)))
                        "active"))
        (let ((epoch (cdr (assq 'epoch skg--maintenance-state))))
          (unless (natnump epoch)
            (error "Active maintenance has no valid epoch"))
          (skg-lock-buffer-for-maintenance buffer epoch)))
      record)))

(defun skg-unregister-current-buffer ()
  (when skg--buffer-record
    (remhash (skg--buffer-record-id skg--buffer-record)
             skg--buffer-registry)))

(defun skg-registered-buffers ()
  "Return live registered buffers, pruning dead entries."
  (let (buffers dead)
    (maphash (lambda (id buffer)
               (if (buffer-live-p buffer)
                   (push buffer buffers)
                 (push id dead)))
             skg--buffer-registry)
    (dolist (id dead) (remhash id skg--buffer-registry))
    (nreverse buffers)))

(defun skg-find-buffer-by-id (buffer-id)
  "Return the live registered buffer named by BUFFER-ID, or nil."
  (let ((buffer (and buffer-id
                     (gethash (format "%s" buffer-id)
                              skg--buffer-registry))))
    (when (buffer-live-p buffer) buffer)))

(defun skg-buffer-dirty-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (or (buffer-modified-p)
        (and skg--buffer-record
             (skg--buffer-record-logical-dirty skg--buffer-record)))))

(defun skg--maintenance-settlement-value (settlement key)
  (cadr (assoc key settlement)))

(defun skg--maintenance-settlement-text (settlement key)
  (when-let ((value (skg--maintenance-settlement-value settlement key)))
    (format "%s" value)))

(defun skg--maintenance-settlement-nat (settlement key)
  (let ((value (skg--maintenance-settlement-value settlement key)))
    (cond
     ((natnump value) value)
     ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
      (string-to-number value))
     (t (error "Maintenance settlement has invalid %s" key)))))

(defun skg--maintenance-settlement-uri (settlement)
  (let ((uri (skg--maintenance-settlement-text settlement 'view-uri)))
    (unless (member uri '(nil "nil" "none")) uri)))

(defun skg-validate-maintenance-buffer-base (buffer settlement epoch)
  "Return BUFFER's record after proving SETTLEMENT's exact frozen base."
  (unless (buffer-live-p buffer)
    (error "Maintenance buffer is no longer live"))
  (with-current-buffer buffer
    (unless skg--buffer-record
      (error "Maintenance buffer is no longer registered"))
    (let ((buffer-id (skg--maintenance-settlement-text
                      settlement 'buffer-id))
          (kind (intern (skg--maintenance-settlement-text settlement 'kind)))
          (uri (skg--maintenance-settlement-uri settlement))
          (dirty (equal (skg--maintenance-settlement-text
                         settlement 'dirty)
                        "true")))
      (unless (and
               (equal epoch
                      (skg--buffer-record-maintenance-epoch
                       skg--buffer-record))
               (equal buffer-id (skg--buffer-record-id skg--buffer-record))
               (eq kind (skg--buffer-record-kind skg--buffer-record))
               (equal uri (skg--buffer-record-view-uri skg--buffer-record))
               (= (skg--maintenance-settlement-nat
                   settlement 'base-graph-generation)
                  (skg--buffer-record-graph-generation skg--buffer-record))
               (= (skg--maintenance-settlement-nat
                   settlement 'base-presentation-generation)
                  (skg--buffer-record-presentation-generation
                   skg--buffer-record))
               (= (skg--maintenance-settlement-nat
                   settlement 'base-server-revision)
                  (skg--buffer-record-server-revision skg--buffer-record))
               (= (skg--maintenance-settlement-nat
                   settlement 'base-application-token)
                  (skg--buffer-record-application-token skg--buffer-record))
               (eq dirty (and (skg-buffer-dirty-p buffer) t)))
        (error "Maintenance buffer %s changed from its frozen authority"
               buffer-id))
      skg--buffer-record)))

(defun skg-release-buffer-across-maintenance
    (buffer settlement epoch graph-generation)
  "Preserve BUFFER exactly while associating it with selected graph G1."
  (with-current-buffer buffer
    (skg-validate-maintenance-buffer-base buffer settlement epoch)
    (setf (skg--buffer-record-graph-generation skg--buffer-record)
          graph-generation
          (skg--buffer-record-presentation-stale skg--buffer-record) t
          (skg--buffer-record-search-stale skg--buffer-record)
          (or (skg--buffer-record-search-stale skg--buffer-record)
              (eq (skg--buffer-record-kind skg--buffer-record)
                  'search-view)))
    skg--buffer-record))

(defun skg-retire-buffer-for-maintenance
    (buffer settlement epoch incident-id)
  "Detach BUFFER from Skg authority without changing text or undo history."
  (with-current-buffer buffer
    (skg-validate-maintenance-buffer-base buffer settlement epoch)
    (remove-hook 'kill-buffer-hook #'skg-send-close-view t)
    (when (boundp 'skg-view-uri) (setq skg-view-uri nil))
    (setf (skg--buffer-record-view-uri skg--buffer-record) nil
          (skg--buffer-record-lifecycle skg--buffer-record)
          'detached-recovery
          (skg--buffer-record-presentation-stale skg--buffer-record) t
          (skg--buffer-record-search-stale skg--buffer-record)
          (or (skg--buffer-record-search-stale skg--buffer-record)
              (eq (skg--buffer-record-kind skg--buffer-record)
                  'search-view)))
    (let* ((short-incident (substring incident-id 0 (min 8 (length incident-id))))
           (short-buffer (substring
                          (skg--buffer-record-id skg--buffer-record)
                          0 (min 8 (length
                                    (skg--buffer-record-id
                                     skg--buffer-record)))))
           (desired (format "%s [recovery %s/%s]"
                            (buffer-name) short-incident short-buffer)))
      (unless (equal desired (buffer-name))
        (rename-buffer (generate-new-buffer-name desired) nil)))
    skg--buffer-record))

(defun skg-close-buffer-for-maintenance (buffer settlement epoch)
  "Close one disposable clean BUFFER after exact settlement validation."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (skg-validate-maintenance-buffer-base buffer settlement epoch)
      (remove-hook 'kill-buffer-hook #'skg-send-close-view t)
      (unless (kill-buffer buffer)
        (error "Maintenance close was refused for buffer %s"
               (skg--maintenance-settlement-text settlement 'buffer-id))))))

(defun skg-apply-server-text
    (buffer text expected-uri expected-token graph-generation
            presentation-generation server-revision)
  "Install TEXT only into the exact clean application record named."
  (with-current-buffer buffer
    (unless skg--buffer-record (error "Skg buffer is not registered"))
    (unless (equal expected-uri (skg--buffer-record-view-uri
                                 skg--buffer-record))
      (error "Skg view URI changed before application"))
    (unless (equal expected-token (skg--buffer-record-application-token
                                   skg--buffer-record))
      (error "Skg application token changed before application"))
    (when (skg-buffer-dirty-p buffer)
      (error "Skg refuses to replace a dirty buffer"))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (save-restriction
        (widen)
        (erase-buffer)
        (insert text)))
    (set-buffer-modified-p nil)
    (setf (skg--buffer-record-last-fetched skg--buffer-record) text
          (skg--buffer-record-last-fetched-sha256 skg--buffer-record)
          (skg--sha256-text text)
          (skg--buffer-record-application-token skg--buffer-record)
          (1+ expected-token)
          (skg--buffer-record-graph-generation skg--buffer-record)
          graph-generation
          (skg--buffer-record-presentation-generation skg--buffer-record)
          presentation-generation
          (skg--buffer-record-server-revision skg--buffer-record)
          server-revision
          (skg--buffer-record-presentation-stale skg--buffer-record) nil)
    (skg--buffer-record-application-token skg--buffer-record)))

(defun skg--maintenance-lock-signal (&rest _)
  (let ((epoch (and skg--buffer-record
                    (skg--buffer-record-maintenance-epoch
                     skg--buffer-record))))
    (error "skg: buffer locked for maintenance epoch %s" epoch)))

(defun skg-lock-buffer-for-maintenance (buffer epoch)
  (with-current-buffer buffer
    (when skg--buffer-record
      (setf (skg--buffer-record-maintenance-epoch skg--buffer-record) epoch)
      (unless skg--maintenance-lock-overlay
        (setq skg--maintenance-lock-overlay
              (make-overlay (point-min) (point-max) buffer))
        (overlay-put skg--maintenance-lock-overlay 'modification-hooks
                     '(skg--maintenance-lock-signal))
        (overlay-put skg--maintenance-lock-overlay 'insert-in-front-hooks
                     '(skg--maintenance-lock-signal))
        (overlay-put skg--maintenance-lock-overlay 'insert-behind-hooks
                     '(skg--maintenance-lock-signal))))))

(defun skg-unlock-buffer-after-maintenance (buffer epoch)
  (with-current-buffer buffer
    (when (and skg--buffer-record
               (equal epoch (skg--buffer-record-maintenance-epoch
                             skg--buffer-record)))
      (when skg--maintenance-lock-overlay
        (delete-overlay skg--maintenance-lock-overlay)
        (setq skg--maintenance-lock-overlay nil))
      (setf (skg--buffer-record-maintenance-epoch skg--buffer-record) nil))))

(defun skg-buffer-status-indicator ()
  (when skg--buffer-record
    (concat
     (when (skg--buffer-record-maintenance-epoch skg--buffer-record)
       (format " M:%s"
               (skg--buffer-record-maintenance-epoch skg--buffer-record)))
     (when (skg--buffer-record-presentation-stale skg--buffer-record)
       " presentation-stale")
     (when (skg--buffer-record-search-stale skg--buffer-record)
       " search-stale"))))

(defun skg-buffer-census ()
  "Return a compact, portable census of registered buffers."
  (mapcar
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((record skg--buffer-record)
             (current (skg-buffer-raw-text buffer)))
         `((buffer-id . ,(skg--buffer-record-id record))
           (kind . ,(symbol-name (skg--buffer-record-kind record)))
           (lifecycle . ,(symbol-name (skg--buffer-record-lifecycle record)))
           (view-uri . ,(or (skg--buffer-record-view-uri record) "nil"))
           (recipe . ,(skg--buffer-record-recipe record))
           (root-ids . ,(skg--buffer-record-root-ids record))
           (source-set . ,(skg--buffer-record-source-set record))
           (graph-generation . ,(or (skg--buffer-record-graph-generation record) 0))
           (presentation-generation
            . ,(or (skg--buffer-record-presentation-generation record) 0))
           (server-revision . ,(skg--buffer-record-server-revision record))
           (application-token . ,(skg--buffer-record-application-token record))
           (dirty . ,(if (skg-buffer-dirty-p buffer) "true" "nil"))
           (undo-required
            . ,(if (and (skg-buffer-dirty-p buffer)
                        (or (eq buffer-undo-list t)
                            (consp buffer-undo-list)
                            (consp pending-undo-list)))
                   "true" "nil"))
           (logical-dirty
            . ,(if (skg--buffer-record-logical-dirty record) "true" "nil"))
           (presentation-stale
            . ,(if (skg--buffer-record-presentation-stale record)
                   "true" "nil"))
           (search-stale
            . ,(if (skg--buffer-record-search-stale record) "true" "nil"))
           (last-fetched-sha256 . ,(skg--buffer-record-last-fetched-sha256 record))
           (current-sha256 . ,(skg--sha256-text current))))))
   (skg-registered-buffers)))

(defun skg-buffer-census-texts (buffer-ids)
  "Return exact last-fetched/current texts for requested BUFFER-IDS."
  (mapcar
   (lambda (buffer-id)
     (let ((buffer (gethash buffer-id skg--buffer-registry)))
       (unless (buffer-live-p buffer)
         (error "Server requested census text for dead buffer %s" buffer-id))
       (with-current-buffer buffer
         `((buffer-id . ,buffer-id)
           (last-fetched
            . ,(skg--buffer-record-last-fetched skg--buffer-record))
           (current . ,(skg-buffer-raw-text buffer))))))
   buffer-ids))

(defun skg-mark-census-buffers-stale (buffer-ids)
  "Preserve BUFFER-IDS but remove their live save authority."
  (dolist (buffer-id buffer-ids)
    (when-let ((buffer (gethash (format "%s" buffer-id)
                                skg--buffer-registry)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setf (skg--buffer-record-presentation-stale skg--buffer-record) t)
          (setq skg-view-uri nil)
          (setf (skg--buffer-record-view-uri skg--buffer-record) nil)))))
  (when buffer-ids
    (display-warning
     'skg
     (format "%d Skg buffer(s) could not be reattached; their text was preserved without live save authority"
             (length buffer-ids))
     :warning)))

(provide 'skg-buffer-registry)
