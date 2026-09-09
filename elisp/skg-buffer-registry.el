;;; -*- lexical-binding: t; -*-
;;; Explicit lifecycle records for every Skg-owned editor buffer.

(require 'cl-lib)
(require 'org-id)
(require 'subr-x)
(require 'skg-state)

(cl-defstruct skg--buffer-record
  id kind lifecycle disposable continuation-id buffer view-uri recipe root-ids
  source-set server-session-id view-write-authority
  graph-generation presentation-generation
  server-revision
  application-token last-fetched last-fetched-sha256 logical-dirty
  origin-buffer-id origin-view-uri origin-application-token origin-location
  attached-workflow-count transient-lock-reasons maintenance-epoch
  presentation-stale search-stale herald-bearing)

(defvar skg--buffer-registry (make-hash-table :test #'equal)
  "Skg-owned buffers keyed by stable client-local buffer ID.")

(defvar skg--pending-maintenance-offer)

(defvar-local skg--buffer-record nil)
(put 'skg--buffer-record 'permanent-local t)

(defvar-local skg--maintenance-lock-overlay nil)
(put 'skg--maintenance-lock-overlay 'permanent-local t)

(defface skg-rebuilding-face
  '((t :foreground "orange"))
  "Face for the compact graph/search rebuilding indicator.")

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

(defun skg--normalized-recipe-value (value)
  "Return VALUE in the portable proper-list recipe grammar."
  (cond
   ((null value) "nil")
   ((eq value t) "true")
   ((stringp value) value)
   ((numberp value) value)
   ((symbolp value) (symbol-name value))
   ((and (proper-list-p value) (cl-every #'consp value))
    (mapcar
     (lambda (entry)
       (let* ((key (replace-regexp-in-string
                    "_" "-" (format "%s" (car entry))))
              (entry-value
               (if (and (proper-list-p entry) (= (length entry) 2))
                   (cadr entry)
                 (cdr entry))))
         (list (intern key) (skg--normalized-recipe-value entry-value))))
     (sort (copy-sequence value)
           (lambda (left right)
             (string< (format "%s" (car left))
                      (format "%s" (car right)))))))
   ((proper-list-p value)
    (mapcar #'skg--normalized-recipe-value value))
   (t (error "Skg buffer recipe contains unsupported value: %S" value))))

(defun skg-buffer-recipe-text (recipe)
  "Return RECIPE's deterministic, language-neutral census spelling."
  (prin1-to-string
   (if recipe (skg--normalized-recipe-value recipe) nil)))

(defun skg--normalized-string-list (values)
  (sort (delete-dups (mapcar (lambda (value) (format "%s" value)) values))
        #'string<))

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
    (buffer kind &key view-uri recipe root-ids
            (lifecycle nil lifecycle-supplied-p)
            (disposable nil disposable-supplied-p)
            continuation-id origin-buffer origin-location last-fetched
            server-revision graph-generation presentation-generation
            application-token server-session-id view-write-authority)
  "Register BUFFER under an explicit KIND and return its durable record."
  (unless lifecycle-supplied-p
    (error "Skg buffer constructor omitted its lifecycle"))
  (unless disposable-supplied-p
    (error "Skg buffer constructor omitted its disposable policy"))
  (with-current-buffer buffer
    (let* ((existing skg--buffer-record)
           (old-origin-id
            (and existing (skg--buffer-record-origin-buffer-id existing)))
           (origin-record
            (when origin-buffer
              (unless (buffer-live-p origin-buffer)
                (error "Skg workflow origin is no longer live"))
              (buffer-local-value 'skg--buffer-record origin-buffer)))
           (_
            (when (and origin-buffer (null origin-record))
              (error "Skg workflow origin is not registered")))
           (id (or (and existing (skg--buffer-record-id existing))
                   (org-id-uuid)))
           (text (or last-fetched (skg-buffer-raw-text buffer)))
           (record (make-skg--buffer-record
                    :id id
                    :kind kind
                    :lifecycle lifecycle
                    :disposable disposable
                    :continuation-id continuation-id
                    :buffer buffer
                    :view-uri view-uri
                    :recipe recipe
                    :root-ids (or root-ids
                                  (skg--conservative-ids-from-text text))
                    :source-set skg--active-source-set-name
                    :server-session-id
                    (or server-session-id
                        (and existing
                             (skg--buffer-record-server-session-id existing))
                        (and origin-record
                             (skg--buffer-record-server-session-id
                              origin-record))
                        skg--server-session-id)
                    :view-write-authority
                    (if (and (null existing)
                             (null view-write-authority)
                             (not (eq skg--client-constructor-admission
                                      'open)))
                        'read-only
                      (or view-write-authority
                          (and existing
                               (skg--buffer-record-view-write-authority
                                existing))
                          (and origin-record
                               (skg--buffer-record-view-write-authority
                                origin-record))
                          'editable))
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
                    :logical-dirty
                    (memq kind '(metadata-editor fork-confirmation
                                  disk-conflict))
                    :origin-buffer-id
                    (and origin-record (skg--buffer-record-id origin-record))
                    :origin-view-uri
                    (and origin-record
                         (skg--buffer-record-view-uri origin-record))
                    :origin-application-token
                    (and origin-record
                         (skg--buffer-record-application-token origin-record))
                    :origin-location origin-location
                    :attached-workflow-count
                    (or (and existing
                             (skg--buffer-record-attached-workflow-count
                              existing))
                        0)
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
      (add-hook 'buffer-list-update-hook
                #'skg-warn-buffer-status-on-entry nil t)
      (setq-local mode-line-process
                  '(:eval (skg-buffer-status-indicator)))
      (when old-origin-id
        (skg--refresh-attached-workflow-count old-origin-id))
      (when origin-record
        (skg--refresh-attached-workflow-count
         (skg--buffer-record-id origin-record)))
      record)))

(defun skg-unregister-current-buffer ()
  (when skg--buffer-record
    (let ((origin-id (skg--buffer-record-origin-buffer-id
                      skg--buffer-record)))
      (remhash (skg--buffer-record-id skg--buffer-record)
               skg--buffer-registry)
      (when origin-id
        (skg--refresh-attached-workflow-count origin-id)))))

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

(defun skg-adopt-unbound-new-empty-authority
    (graph-generation source-set server-session-id)
  "Initialize never-connected new-empty records from their first handshake.
Records which already name a graph generation retain it: reconnect must not
silently rebase genuinely stale client work."
  (dolist (buffer (skg-registered-buffers))
    (with-current-buffer buffer
      (when (and skg--buffer-record
                 (eq (skg--buffer-record-kind skg--buffer-record)
                     'new-empty-content-view)
                 (null (skg--buffer-record-graph-generation
                        skg--buffer-record))
                 (null (skg--buffer-record-server-session-id
                        skg--buffer-record))
                 (= (skg--buffer-record-server-revision
                     skg--buffer-record) 0)
                 (= (skg--buffer-record-application-token
                     skg--buffer-record) 1))
        (setf (skg--buffer-record-graph-generation skg--buffer-record)
              graph-generation
              (skg--buffer-record-source-set skg--buffer-record)
              source-set
              (skg--buffer-record-server-session-id skg--buffer-record)
              server-session-id)))))

(defun skg-acquire-generated-buffer (name)
  "Return an explicitly reusable buffer NAME, preserving every other namesake."
  (let ((existing (get-buffer name)))
    (if (and (buffer-live-p existing)
             (buffer-local-value 'skg--buffer-record existing)
             (skg--buffer-record-disposable
              (buffer-local-value 'skg--buffer-record existing))
             (not (skg--buffer-record-continuation-id
                   (buffer-local-value 'skg--buffer-record existing)))
             (not (skg--buffer-record-maintenance-epoch
                   (buffer-local-value 'skg--buffer-record existing)))
             (not (buffer-modified-p existing))
             (not (skg-buffer-logical-dirty-p existing)))
        existing
      (generate-new-buffer name))))

(defun skg--attached-workflow-record-p (record origin-id)
  (and record
       (equal (skg--buffer-record-origin-buffer-id record) origin-id)
       (memq (skg--buffer-record-kind record)
             '(metadata-editor fork-confirmation disk-conflict))))

(defun skg--refresh-attached-workflow-count (origin-id)
  "Recompute the unfinished workflow count attached to ORIGIN-ID."
  (when-let ((origin (skg-find-buffer-by-id origin-id)))
    (let ((count 0))
      (dolist (buffer (skg-registered-buffers))
        (when (and (not (eq buffer origin))
                   (skg--attached-workflow-record-p
                    (buffer-local-value 'skg--buffer-record buffer)
                    origin-id))
          (setq count (1+ count))))
      (with-current-buffer origin
        (setf (skg--buffer-record-attached-workflow-count
               skg--buffer-record)
              count)))))

(defun skg-buffer-logical-dirty-p (&optional buffer)
  "Whether BUFFER has local logical state not represented by modified-p."
  (with-current-buffer (or buffer (current-buffer))
    (and skg--buffer-record
         (or (skg--buffer-record-logical-dirty skg--buffer-record)
             (> (or (skg--buffer-record-attached-workflow-count
                     skg--buffer-record)
                    0)
                0)))))

(defun skg-buffer-dirty-p (&optional buffer)
  "Whether BUFFER contains state maintenance must preserve as dirty work.
Reload selectors hold only transient command input, never authored state."
  (with-current-buffer (or buffer (current-buffer))
    (and (not (and skg--buffer-record
                   (eq (skg--buffer-record-kind skg--buffer-record)
                       'reload-selector)))
         (or (buffer-modified-p)
             (skg-buffer-logical-dirty-p)))))

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
    (let ((record (skg-validate-maintenance-buffer-base
                   buffer settlement epoch)))
      (when (skg--buffer-record-origin-buffer-id record)
        (skg--validate-attached-workflow-origin record)))
    (setf (skg--buffer-record-graph-generation skg--buffer-record)
          graph-generation
          (skg--buffer-record-presentation-stale skg--buffer-record) t
          (skg--buffer-record-search-stale skg--buffer-record)
          (or (skg--buffer-record-search-stale skg--buffer-record)
              (eq (skg--buffer-record-kind skg--buffer-record)
                  'search-view)))
    skg--buffer-record))

(defun skg--validate-attached-workflow-origin (record)
  "Require RECORD's exact originating buffer authority to remain live."
  (let* ((origin-id (skg--buffer-record-origin-buffer-id record))
         (origin (skg-find-buffer-by-id origin-id)))
    (unless (buffer-live-p origin)
      (error "Attached workflow origin %s is no longer live" origin-id))
    (let ((origin-record (buffer-local-value 'skg--buffer-record origin)))
      (unless (and origin-record
                   (equal (skg--buffer-record-view-uri origin-record)
                          (skg--buffer-record-origin-view-uri record))
                   (equal (skg--buffer-record-application-token origin-record)
                          (skg--buffer-record-origin-application-token record))
                   (stringp (skg--buffer-record-origin-location record)))
        (error "Attached workflow origin authority changed")))))

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
      (unless (and (skg--buffer-record-disposable skg--buffer-record)
                   (not (skg-buffer-dirty-p buffer)))
        (error "Maintenance refuses to close a non-disposable or dirty buffer"))
      (when (skg--buffer-record-continuation-id skg--buffer-record)
        (unless (eq (skg--buffer-record-kind skg--buffer-record)
                    'relationship-kind-menu)
          (error "Maintenance refuses to close a live continuation"))
        (when (boundp 'skg--relationship-kind-menu-continuation)
          (setq skg--relationship-kind-menu-continuation nil))
        (setf (skg--buffer-record-continuation-id skg--buffer-record) nil))
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
      (when (and (memq (skg--buffer-record-lifecycle skg--buffer-record)
                       '(live-view attached-workflow maintenance-control
                         ordinary-file))
                 (not skg--maintenance-lock-overlay))
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
      (when (and (eq (skg--buffer-record-kind skg--buffer-record)
                     'raw-skg-file)
                 (fboundp 'skg-refresh-raw-file-disk-staleness))
        (skg-refresh-raw-file-disk-staleness buffer))
      (let ((origin-id
             (and (eq (skg--buffer-record-lifecycle skg--buffer-record)
                      'detached-recovery)
                  (skg--buffer-record-origin-buffer-id skg--buffer-record))))
        (setf (skg--buffer-record-maintenance-epoch skg--buffer-record) nil)
        (when origin-id
          (setf (skg--buffer-record-continuation-id skg--buffer-record) nil
                (skg--buffer-record-origin-buffer-id skg--buffer-record) nil
                (skg--buffer-record-origin-view-uri skg--buffer-record) nil
                (skg--buffer-record-origin-application-token
                 skg--buffer-record)
                nil
                (skg--buffer-record-origin-location skg--buffer-record) nil)
          (skg--refresh-attached-workflow-count origin-id))))))

(defun skg-buffer-status-indicator ()
  (when skg--buffer-record
    (concat
     (when skg--rebuilding
       (concat " " (propertize "rebuilding" 'face 'skg-rebuilding-face)))
     (when (and (boundp 'skg--pending-maintenance-offer)
                skg--pending-maintenance-offer)
       " pending-disk")
     (when (skg--buffer-record-maintenance-epoch skg--buffer-record)
       (format " M:%s"
               (skg--buffer-record-maintenance-epoch skg--buffer-record)))
     (when (skg--buffer-record-presentation-stale skg--buffer-record)
       " presentation-stale")
     (when (skg--buffer-record-search-stale skg--buffer-record)
       " search-stale")
     (when (and (boundp 'skg--raw-file-externally-stale)
                skg--raw-file-externally-stale)
       " external-file-stale"))))

(defun skg-known-save-restriction (&optional buffer)
  "Return the known reason BUFFER cannot begin a save, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     (skg--rebuilding
      "the graph and search index are rebuilding")
     ((and skg--buffer-record
           (skg--buffer-record-maintenance-epoch skg--buffer-record))
      (format "this buffer is maintenance-locked for epoch %s"
              (skg--buffer-record-maintenance-epoch skg--buffer-record)))
     ((and (boundp 'skg--pending-maintenance-offer)
           skg--pending-maintenance-offer)
      "disk reconciliation is pending")
     ((and skg--server-session-id skg--buffer-record
           (not (equal skg--server-session-id
                       (skg--buffer-record-server-session-id
                        skg--buffer-record))))
      "this buffer belongs to an earlier server session; reopen it")
     ((and skg--buffer-record
           (not (eq (skg--buffer-record-view-write-authority
                     skg--buffer-record)
                    'editable)))
      "this view has read-only result authority; reopen it explicitly"))))

(defun skg-buffer-status-messages ()
  "Return warnings which remain relevant to the current Skg buffer."
  (when skg--buffer-record
    (delq
     nil
     (list
      (when (and (boundp 'skg--pending-maintenance-offer)
                 skg--pending-maintenance-offer)
        "Disk reconciliation is pending; every Skg view save is blocked")
      (when-let ((epoch (skg--buffer-record-maintenance-epoch
                         skg--buffer-record)))
        (format "This Skg buffer is maintenance-locked for epoch %s" epoch))
      (when (skg--buffer-record-presentation-stale skg--buffer-record)
        (if (skg--buffer-record-herald-bearing skg--buffer-record)
            "This preserved presentation is stale; generated heralds may describe an older graph"
          "This preserved presentation may describe an older graph"))
      (when (skg--buffer-record-search-stale skg--buffer-record)
        "Search membership and ranking are stale; rerun the search explicitly")
      (when (and skg--server-session-id
                 (not (equal skg--server-session-id
                             (skg--buffer-record-server-session-id
                              skg--buffer-record))))
        (concat "This buffer belongs to an earlier server session; "
                "reopen it before saving"))
      (when (not (eq (skg--buffer-record-view-write-authority
                      skg--buffer-record)
                     'editable))
        "This result is read-only; reopen it explicitly to gain save authority")
      (when (and (boundp 'skg--raw-file-externally-stale)
                 skg--raw-file-externally-stale)
        "This raw .skg file changed on disk; revert or reconcile before saving")))))

(defun skg-warn-buffer-status-on-entry ()
  "Repeat persistent maintenance and staleness warnings on buffer entry."
  (when-let ((messages (skg-buffer-status-messages)))
    (message "Skg: %s" (string-join messages "; "))))

(cl-defun skg-buffer-census (&optional (buffers nil buffers-supplied-p))
  "Return a compact, portable census of registered buffers."
  (mapcar
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((record skg--buffer-record)
             (current (skg-buffer-raw-text buffer)))
         `((buffer-id . ,(skg--buffer-record-id record))
           (kind . ,(symbol-name (skg--buffer-record-kind record)))
           (lifecycle . ,(symbol-name (skg--buffer-record-lifecycle record)))
           (disposable
            . ,(if (skg--buffer-record-disposable record) "true" "nil"))
           (continuation-id
            . ,(or (skg--buffer-record-continuation-id record) "nil"))
           (origin-buffer-id
            . ,(or (skg--buffer-record-origin-buffer-id record) "nil"))
           (origin-view-uri
            . ,(or (skg--buffer-record-origin-view-uri record) "nil"))
           (origin-application-token
            . ,(or (skg--buffer-record-origin-application-token record) "nil"))
           (origin-location
            . ,(or (skg--buffer-record-origin-location record) "nil"))
           (view-uri . ,(or (skg--buffer-record-view-uri record) "nil"))
           (recipe . ,(skg-buffer-recipe-text
                       (skg--buffer-record-recipe record)))
           (root-ids
            ,(skg--normalized-string-list
              (skg--buffer-record-root-ids record)))
           (source-set . ,(or (skg--buffer-record-source-set record) "all"))
           (server-session-id
            . ,(or (skg--buffer-record-server-session-id record) "nil"))
           (view-write-authority
            . ,(symbol-name
                (skg--buffer-record-view-write-authority record)))
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
            . ,(if (skg-buffer-logical-dirty-p buffer) "true" "nil"))
           (maintenance-epoch
            . ,(or (skg--buffer-record-maintenance-epoch record) "nil"))
           (modification-tick . ,(buffer-chars-modified-tick))
           (presentation-stale
            . ,(if (skg--buffer-record-presentation-stale record)
                   "true" "nil"))
           (search-stale
            . ,(if (skg--buffer-record-search-stale record) "true" "nil"))
           (herald-bearing
            . ,(if (skg--buffer-record-herald-bearing record) "true" "nil"))
           (last-fetched-sha256 . ,(skg--buffer-record-last-fetched-sha256 record))
           (current-sha256 . ,(skg--sha256-text current))))))
   (if buffers-supplied-p buffers (skg-registered-buffers))))

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

(defun skg-mark-view-uris-presentation-stale (view-uris)
  "Mark registered live VIEW-URIS stale without changing their save authority."
  (let ((wanted (mapcar (lambda (uri) (format "%s" uri)) view-uris))
        changed)
    (dolist (buffer (skg-registered-buffers))
      (with-current-buffer buffer
        (when (and skg--buffer-record
                   (member (skg--buffer-record-view-uri skg--buffer-record)
                           wanted))
          (setf (skg--buffer-record-presentation-stale skg--buffer-record) t
                changed t))))
    (when changed (force-mode-line-update t))))

(provide 'skg-buffer-registry)
