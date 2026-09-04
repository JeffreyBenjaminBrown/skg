;;; skg-recovery-archive.el --- Private maintenance recovery archives -*- lexical-binding: t; -*-

;; This module publishes client-owned buffer evidence only.  It never mutates
;; a source, Git repository, or selected server store.  The returned manifest
;; checksum is the exact boundary the server must acknowledge before doing so.

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer-registry)
(require 'skg-config)
(require 'skg-state)
(require 'skg-undo-sidecar)

(define-error 'skg-recovery-archive-error "Skg recovery archive failed")
(define-error 'skg-recovery-native-undo-error
  "Skg native undo archive failed" 'skg-recovery-archive-error)

(defconst skg-recovery-archive-format-version 1)

(defvar skg-recovery-archive-native-undo-function
  #'skg-undo-sidecar-save
  "Function used to write and verify one native Emacs undo sidecar.")

(defun skg-recovery--fail (format-string &rest arguments)
  (signal 'skg-recovery-archive-error
          (list (apply #'format format-string arguments))))

(defun skg-recovery--field (name value)
  (list name value))

(defun skg-recovery--canonical-quote (value)
  (concat
   "\""
   (mapconcat
    (lambda (character)
      (pcase character
        (?\\ "\\\\")
        (?\" "\\\"")
        (?\n "\\n")
        (?\t "\\t")
        (_ (char-to-string character))))
    value "")
   "\""))

(defun skg-recovery-canonical-sexpr (value)
  "Render VALUE in the portable archive's proper-list grammar."
  (cond
   ((null value) "()")
   ((stringp value) (skg-recovery--canonical-quote value))
   ((symbolp value) (symbol-name value))
   ((integerp value) (number-to-string value))
   ((and (listp value) (proper-list-p value))
    (concat "("
            (mapconcat #'skg-recovery-canonical-sexpr value " ")
            ")"))
   (t (skg-recovery--fail
       "manifest contains unsupported value: %S" value))))

(defun skg-recovery--path-beneath-p (child parent)
  (let ((child (directory-file-name (expand-file-name child)))
        (parent (directory-file-name (expand-file-name parent))))
    (or (equal child parent)
        (string-prefix-p (file-name-as-directory parent)
                         (file-name-as-directory child)))))

(defun skg-recovery--reject-parent-components (path)
  (when (member ".." (split-string path "[/\\]+" t))
    (skg-recovery--fail
     "maintenance_archive_folder may not contain '..'")))

(defun skg-recovery--require-directory (path description)
  (when (file-symlink-p path)
    (skg-recovery--fail "%s is a symlink: %s" description path))
  (unless (file-directory-p path)
    (skg-recovery--fail "%s is not a directory: %s" description path))
  (when (and (file-modes path) (/= (logand (file-modes path) #o777) #o700))
    (skg-recovery--fail "%s is not private (expected mode 0700): %s"
                        description path)))

(defun skg-recovery--make-private-directory (path &optional allow-existing)
  (cond
   ((file-exists-p path)
    (unless allow-existing
      (skg-recovery--fail "archive directory already exists: %s" path))
    (when (file-symlink-p path)
      (skg-recovery--fail "archive directory is a symlink: %s" path))
    (unless (file-directory-p path)
      (skg-recovery--fail "archive path is not a directory: %s" path))
    (set-file-modes path #o700))
   (t
    (with-file-modes #o700
      (make-directory path nil))))
  (skg-recovery--require-directory path "archive directory"))

(defun skg-recovery--local-source-identities ()
  (let ((config-file (skg-config-file)))
    (unless config-file
      (skg-recovery--fail "no local skgconfig.toml is active"))
    (cons
     config-file
     (mapcar
      (lambda (source)
        (cons (car source)
              (if (file-exists-p (cdr source))
                  (file-truename (cdr source))
                (expand-file-name (cdr source)))))
      (skg-source-paths-from-toml config-file)))))

(defun skg-recovery-resolve-archive-root (&optional configured)
  "Resolve and validate CONFIGURED in this editor's filesystem view."
  (setq configured (or configured skg--maintenance-archive-folder))
  (unless (and (stringp configured) (not (string-empty-p configured)))
    (skg-recovery--fail "the server supplied no maintenance archive folder"))
  (skg-recovery--reject-parent-components configured)
  (pcase-let* ((`(,config-file . ,sources)
                (skg-recovery--local-source-identities))
               (proposed (expand-file-name
                          configured (file-name-directory config-file))))
    (dolist (source sources)
      (when (or (skg-recovery--path-beneath-p proposed (cdr source))
                (skg-recovery--path-beneath-p (cdr source) proposed))
        (skg-recovery--fail "archive root and source '%s' overlap"
                            (car source))))
    (with-file-modes #o700
      (make-directory proposed t))
    (set-file-modes proposed #o700)
    (let ((root (directory-file-name (file-truename proposed))))
      (dolist (source sources)
        (when (or (skg-recovery--path-beneath-p root (cdr source))
                  (skg-recovery--path-beneath-p (cdr source) root))
          (skg-recovery--fail "resolved archive root and source '%s' overlap"
                              (car source))))
      (skg-recovery--require-directory root "archive root")
      root)))

(defun skg-recovery--assert-confined-parent (path incident-root)
  (let* ((root (directory-file-name (expand-file-name incident-root)))
         (target (expand-file-name path))
         (parent (file-name-directory target)))
    (unless (and (not (equal target root))
                 (skg-recovery--path-beneath-p target root))
      (skg-recovery--fail "artifact path escapes incident: %s" target))
    (skg-recovery--require-directory root "incident directory")
    (let ((cursor root)
          (relative (file-relative-name parent root)))
      (dolist (component (split-string relative "[/\\]+" t))
        (setq cursor (expand-file-name component cursor))
        (skg-recovery--require-directory cursor
                                         "incident path component")))
    target))

(defun skg-recovery--read-bytes (path)
  (when (file-symlink-p path)
    (skg-recovery--fail "artifact is a symlink: %s" path))
  (unless (file-regular-p path)
    (skg-recovery--fail "artifact is not a regular file: %s" path))
  (let ((expected-size (file-attribute-size (file-attributes path 'integer))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (unless (= (buffer-size) expected-size)
        (skg-recovery--fail "artifact changed while being read: %s" path))
      (buffer-string))))

(defun skg-recovery--write-private-file (path bytes incident-root)
  (setq path (skg-recovery--assert-confined-parent path incident-root))
  (when (multibyte-string-p bytes)
    (skg-recovery--fail
     "internal archive writer received characters instead of bytes: %s" path))
  (when (file-exists-p path)
    (skg-recovery--fail "artifact already exists: %s" path))
  (let ((coding-system-for-write 'no-conversion)
        (write-region-inhibit-fsync nil))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert bytes)
      (with-file-modes #o600
        (write-region (point-min) (point-max) path nil 'silent nil 'excl))))
  (when (or (file-symlink-p path)
            (not (file-regular-p path))
            (and (file-modes path)
                 (/= (logand (file-modes path) #o777) #o600)))
    (skg-recovery--fail "new artifact is not a private regular file: %s" path))
  (let ((reread (skg-recovery--read-bytes path)))
    (unless (and (= (length reread) (length bytes))
                 (equal reread bytes))
      (skg-recovery--fail "artifact failed exact reread: %s" path))))

(defun skg-recovery--artifact-record (path incident-root)
  (let ((bytes (skg-recovery--read-bytes path)))
    (list
     (skg-recovery--field
      'path (file-relative-name path incident-root))
     (skg-recovery--field 'bytes (length bytes))
     (skg-recovery--field 'sha256 (secure-hash 'sha256 bytes)))))

(defun skg-recovery--strict-uuid-p (value)
  (and (stringp value)
       (string-match-p
        "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
        value)))

(defun skg-recovery--validate-archive-name (name incident-id)
  (unless (and
           (stringp name)
           (string-match
            "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\.[0-9]\\{6\\}Z_\\([0-9a-f-]+\\)\\'"
            name)
           (equal (match-string 1 name) incident-id))
    (skg-recovery--fail "invalid server-owned archive directory name: %S"
                        name)))

(defun skg-recovery--new-nonce ()
  (substring
   (secure-hash 'sha256
                (format "%s:%s:%s" (float-time) (random) (emacs-pid)))
   0 24))

(defun skg-recovery--safe-primary-root (roots)
  (when (= (length roots) 1)
    (let ((root (format "%s" (car roots))))
      (when (and (<= (length root) 80)
                 (not (member root '("." "..")))
                 (string-match-p
                  "\\`[A-Za-z0-9][A-Za-z0-9._-]*\\'" root))
        root))))

(defun skg-recovery--recipe-text (recipe)
  (let ((print-circle t)
        (print-level nil)
        (print-length nil))
    (prin1-to-string (or recipe nil))))

(defun skg-recovery--allocate-buffer-keys (buffers)
  (let* ((descriptors
          (mapcar
           (lambda (buffer)
             (with-current-buffer buffer
               (unless skg--buffer-record
                 (skg-recovery--fail
                  "attempted to archive an unregistered buffer"))
               (list :buffer buffer :record skg--buffer-record)))
           buffers))
         (descriptors
          (sort descriptors
                (lambda (left right)
                  (string<
                   (skg--buffer-record-id (plist-get left :record))
                   (skg--buffer-record-id (plist-get right :record))))))
         (used (make-hash-table :test #'equal))
         (index 0))
    (dolist (descriptor descriptors)
      (cl-incf index)
      (let* ((record (plist-get descriptor :record))
             (identity
              (mapconcat
               #'identity
               (list (skg--buffer-record-id record)
                     (symbol-name (skg--buffer-record-kind record))
                     (or (skg--buffer-record-view-uri record) "")
                     (mapconcat (lambda (root) (format "%s" root))
                                (or (skg--buffer-record-root-ids record) nil)
                                "\0")
                     (skg-recovery--recipe-text
                      (skg--buffer-record-recipe record)))
               "\0"))
             (prefix
              (or (skg-recovery--safe-primary-root
                   (or (skg--buffer-record-root-ids record) nil))
                  (format "view-%d" index)))
             (base (format "%s_%s" prefix
                           (substring (secure-hash 'sha256
                                                   (encode-coding-string
                                                    identity 'utf-8-unix))
                                      0 12)))
             (key base)
             (suffix 1))
        (while (gethash key used)
          (cl-incf suffix)
          (setq key (format "%s-%d" base suffix)))
        (puthash key t used)
        (plist-put descriptor :key key)))
    descriptors))

(defun skg-recovery--fold-state (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let (folds)
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (overlay-get overlay 'invisible)
            (push
             (list (overlay-start overlay)
                   (overlay-end overlay)
                   (format "%s" (or (overlay-get overlay 'category)
                                     (overlay-get overlay 'invisible))))
             folds)))
        (sort folds (lambda (left right) (< (car left) (car right))))))))

(defun skg-recovery--window-state (buffer)
  (let ((ordinal 0)
        windows)
    (dolist (window (get-buffer-window-list buffer nil t))
      (cl-incf ordinal)
      (push
       (list
        (skg-recovery--field 'ordinal ordinal)
        (skg-recovery--field 'point (window-point window))
        (skg-recovery--field 'window-start (window-start window)))
       windows))
    (nreverse windows)))

(defun skg-recovery--unified-diff (old-path new-path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion)
          (status
           (call-process
            "diff" nil t nil "-U" "3"
            "--label" "last-fetched.org"
            "--label" "unsaved-changes.org"
            old-path new-path)))
      (unless (memq status '(0 1))
        (skg-recovery--fail "diff failed with status %S: %s"
                            status (buffer-string)))
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
        (replace-match "\n" t t))
      (buffer-string))))

(defun skg-recovery--plist-value (plist key fallback)
  (if (plist-member plist key) (plist-get plist key) fallback))

(defun skg-recovery--undo-fields (result)
  (let ((fields
         (list
          (skg-recovery--field
           'status (symbol-name (skg-recovery--plist-value
                                 result :status 'unknown)))
          (skg-recovery--field
           'kind (symbol-name (skg-recovery--plist-value
                               result :kind 'unknown)))
          (skg-recovery--field
           'version (format "%s" (skg-recovery--plist-value
                                   result :version "unknown"))))))
    (dolist (pair '((:compression . compression)
                    (:validation . validation)
                    (:bytes . bytes)
                    (:sha256 . sha256)
                    (:reason . reason)))
      (when (plist-member result (car pair))
        (setq fields
              (append
               fields
               (list
                (skg-recovery--field
                 (cdr pair)
                 (let ((value (plist-get result (car pair))))
                   (if (symbolp value) (symbol-name value) value))))))))
    fields))

(defun skg-recovery--root-values (roots)
  (mapcar (lambda (root) (format "%s" root)) (or roots nil)))

(defun skg-recovery--buffer-metadata (descriptor undo windows folds)
  (let* ((buffer (plist-get descriptor :buffer))
         (record (plist-get descriptor :record)))
    (list
     (skg-recovery--field 'archive-format-version
                          skg-recovery-archive-format-version)
     (skg-recovery--field 'buffer-key (plist-get descriptor :key))
     (skg-recovery--field 'buffer-id (skg--buffer-record-id record))
     (skg-recovery--field 'kind
                          (symbol-name (skg--buffer-record-kind record)))
     (skg-recovery--field
      'lifecycle (symbol-name (skg--buffer-record-lifecycle record)))
     (skg-recovery--field 'name (buffer-name buffer))
     (skg-recovery--field
      'view-uri (or (skg--buffer-record-view-uri record) "none"))
     (skg-recovery--field
      'root-ids (skg-recovery--root-values
                 (skg--buffer-record-root-ids record)))
     (skg-recovery--field
      'recipe (skg-recovery--recipe-text
               (skg--buffer-record-recipe record)))
     (skg-recovery--field
      'source-set (or (skg--buffer-record-source-set record) "all"))
     (skg-recovery--field
      'graph-generation
      (or (skg--buffer-record-graph-generation record) 0))
     (skg-recovery--field
      'presentation-generation
      (or (skg--buffer-record-presentation-generation record) 0))
     (skg-recovery--field
      'server-revision (or (skg--buffer-record-server-revision record) 0))
     (skg-recovery--field
      'application-token (or (skg--buffer-record-application-token record) 0))
     (skg-recovery--field
      'dirty (if (skg-buffer-dirty-p buffer) "true" "nil"))
     (skg-recovery--field
      'logical-dirty
      (if (skg--buffer-record-logical-dirty record) "true" "nil"))
     (skg-recovery--field
      'point
      (if windows (car windows)
        (list (skg-recovery--field
               'point (with-current-buffer buffer (point))))))
     (skg-recovery--field 'windows windows)
     (skg-recovery--field 'folds folds)
     (skg-recovery--field 'undo (skg-recovery--undo-fields undo))
     (skg-recovery--field 'initial-disposition "pending-classification"))))

(defun skg-recovery--buffer-readme (descriptor undo)
  (let* ((buffer (plist-get descriptor :buffer))
         (record (plist-get descriptor :record)))
    (format
     (concat
      "* Skg recovery snapshot\n\n"
      "This directory preserves a buffer exactly as it existed when maintenance began.\n\n"
      "- Buffer key: =%s=,\n"
      "- Buffer kind: =%s=,\n"
      "- Original editor name: =%s=,\n"
      "- Native undo status: =%s=.\n\n"
      "Open =unsaved-changes.org= for the authored text.  =last-fetched.org=\n"
      "is its exact server-rendered base and =diff.txt= is the portable fallback.\n")
     (plist-get descriptor :key)
     (skg--buffer-record-kind record)
     (buffer-name buffer)
     (skg-recovery--plist-value undo :status 'unknown))))

(defun skg-recovery--snapshot-one-buffer
    (descriptor staging waiver-reason)
  (let* ((buffer (plist-get descriptor :buffer))
         (record (plist-get descriptor :record))
         (buffer-id (skg--buffer-record-id record))
         (directory (expand-file-name
                     (concat "buffer-snapshots/"
                             (plist-get descriptor :key))
                     staging))
         (last-path (expand-file-name "last-fetched.org" directory))
         (current-path (expand-file-name "unsaved-changes.org" directory))
         (diff-path (expand-file-name "diff.txt" directory))
         (metadata-path (expand-file-name "metadata.sexp" directory))
         (readme-path (expand-file-name "README.org" directory))
         (last-fetched (skg--buffer-record-last-fetched record))
         current undo windows folds artifacts)
    (unless (skg--buffer-record-maintenance-epoch record)
      (skg-recovery--fail "buffer %s is not maintenance-locked" buffer-id))
    (unless (skg-buffer-dirty-p buffer)
      (skg-recovery--fail "buffer %s is no longer dirty" buffer-id))
    (unless (stringp last-fetched)
      (skg-recovery--fail "buffer %s has no exact last-fetched text" buffer-id))
    (skg-recovery--make-private-directory directory)
    (setq current (skg-buffer-raw-text buffer))
    (skg-recovery--write-private-file
     last-path (skg--utf8-unix-bytes last-fetched) staging)
    (skg-recovery--write-private-file
     current-path (skg--utf8-unix-bytes current) staging)
    (skg-recovery--write-private-file
     diff-path (skg-recovery--unified-diff last-path current-path) staging)
    (setq undo
          (if waiver-reason
              `(:status undo-unavailable-approved
                :kind undo-fu-session
                :version ,(or (skg-undo-sidecar-package-version)
                              "unavailable")
                :reason ,waiver-reason)
            (condition-case error-data
                (funcall skg-recovery-archive-native-undo-function
                         buffer current-path
                         (expand-file-name "undo.emacs.gz" directory)
                         staging)
              (error
               (signal 'skg-recovery-native-undo-error
                       (list buffer-id (plist-get descriptor :key)
                             (error-message-string error-data)))))))
    (setq windows (skg-recovery--window-state buffer)
          folds (skg-recovery--fold-state buffer))
    (skg-recovery--write-private-file
     metadata-path
     (skg--utf8-unix-bytes
      (concat
       (skg-recovery-canonical-sexpr
        (skg-recovery--buffer-metadata descriptor undo windows folds))
       "\n"))
     staging)
    (skg-recovery--write-private-file
     readme-path
     (skg--utf8-unix-bytes
      (skg-recovery--buffer-readme descriptor undo))
     staging)
    (unless (equal current (skg-buffer-raw-text buffer))
      (skg-recovery--fail
       "buffer changed while its archive was being written: %s" buffer-id))
    (dolist (path (list readme-path metadata-path last-path current-path
                        diff-path))
      (push (skg-recovery--artifact-record path staging) artifacts))
    (when (eq (plist-get undo :status) 'archived)
      (push (skg-recovery--artifact-record
             (expand-file-name "undo.emacs.gz" directory) staging)
            artifacts))
    (list
     (skg-recovery--field 'buffer-key (plist-get descriptor :key))
     (skg-recovery--field 'buffer-id buffer-id)
     (skg-recovery--field 'kind
                          (symbol-name (skg--buffer-record-kind record)))
     (skg-recovery--field 'name (buffer-name buffer))
     (skg-recovery--field
      'view-uri (or (skg--buffer-record-view-uri record) "none"))
     (skg-recovery--field
      'root-ids (skg-recovery--root-values
                 (skg--buffer-record-root-ids record)))
     (skg-recovery--field
      'recipe (skg-recovery--recipe-text
               (skg--buffer-record-recipe record)))
     (skg-recovery--field
      'graph-generation (or (skg--buffer-record-graph-generation record) 0))
     (skg-recovery--field
      'presentation-generation
      (or (skg--buffer-record-presentation-generation record) 0))
     (skg-recovery--field
      'server-revision (or (skg--buffer-record-server-revision record) 0))
     (skg-recovery--field
      'application-token (or (skg--buffer-record-application-token record) 0))
     (skg-recovery--field 'undo (skg-recovery--undo-fields undo))
     (skg-recovery--field 'artifacts (nreverse artifacts))
     (skg-recovery--field 'initial-disposition "pending-classification"))))

(defun skg-recovery--offer-value (offer key &optional fallback)
  (let ((entry (assq key offer)))
    (if entry
        (if (and (proper-list-p entry) (= (length entry) 2))
            (cadr entry)
          (cdr entry))
      fallback)))

(defun skg-recovery--incident-readme (offer descriptor-count)
  (format
   (concat
    "* Skg maintenance recovery incident\n\n"
    "Incident =%s= began for =%s=.\n"
    "Its initial archive protects %d dirty buffer(s).\n\n"
    "No source file, Git state, or selected Skg store is changed by this archive.\n"
    "The server may cross the maintenance point of no return only after it ACKs\n"
    "the exact checksum in =ARCHIVE-READY=.\n")
   (skg-recovery--offer-value offer 'incident-id)
   (skg-recovery--offer-value offer 'origin)
   descriptor-count))

(defun skg-recovery--initial-manifest
    (offer root buffer-records root-artifacts)
  (list
   (skg-recovery--field 'archive-format-version
                        skg-recovery-archive-format-version)
   (skg-recovery--field 'manifest-kind "initial")
   (skg-recovery--field
    'incident-id (skg-recovery--offer-value offer 'incident-id))
   (skg-recovery--field
    'maintenance-epoch (skg-recovery--offer-value offer 'epoch))
   (skg-recovery--field 'origin
                        (skg-recovery--offer-value offer 'origin))
   (skg-recovery--field
    'started-at-utc (skg-recovery--offer-value offer 'started-at-utc))
   (skg-recovery--field
    'archive-directory-name
    (skg-recovery--offer-value offer 'archive-name))
   (skg-recovery--field 'client-kind "emacs")
   (skg-recovery--field 'client-version emacs-version)
   (skg-recovery--field 'client-session-id skg--client-session-id)
   (skg-recovery--field 'client-archive-identity root)
   (skg-recovery--field
    'server-archive-identity
    (or skg--maintenance-archive-identity "unavailable"))
   (skg-recovery--field
    'source-set
    (or (skg-recovery--offer-value offer 'source-set)
        skg--active-source-set-name "all"))
   (skg-recovery--field
    'g0-graph-generation
    (or (skg-recovery--offer-value offer 'graph-generation) 0))
   (skg-recovery--field
    'g0-manifest-revision
    (or (skg-recovery--offer-value offer 'manifest-revision) 0))
   (skg-recovery--field 'directory-sync
                        (if (fboundp 'unix-sync) "unix-sync" "unavailable"))
   (skg-recovery--field 'artifacts root-artifacts)
   (skg-recovery--field 'buffers buffer-records)
   (skg-recovery--field 'initial-status "prepared-for-publication")))

(defun skg-recovery--sync-filesystem ()
  (when (fboundp 'unix-sync) (unix-sync)))

(defun skg-recovery--marker-value (offer manifest-sha256)
  (list
   (skg-recovery--field 'archive-format-version
                        skg-recovery-archive-format-version)
   (skg-recovery--field
    'incident-id (skg-recovery--offer-value offer 'incident-id))
   (skg-recovery--field 'manifest-sha256 manifest-sha256)))

(defun skg-recovery--strict-final-name-p (name)
  (and (string-match
        "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\.[0-9]\\{6\\}Z_\\([0-9a-f-]+\\)\\'"
        name)
       (skg-recovery--strict-uuid-p (match-string 1 name))))

(defun skg-recovery--tree-bytes (root)
  (let ((total 0))
    (cl-labels
        ((walk
          (directory)
          (dolist (path (directory-files
                         directory t directory-files-no-dot-files-regexp t))
            (let ((attributes (file-attributes path 'integer)))
              (unless attributes
                (skg-recovery--fail
                 "archive entry vanished during size walk: %s" path))
              (pcase (file-attribute-type attributes)
                ((pred stringp)
                 (skg-recovery--fail
                  "archive size walk found symlink: %s" path))
                ('t (walk path))
                ('nil
                 (unless (file-regular-p path)
                   (skg-recovery--fail
                    "archive size walk found special entry: %s" path))
                 (cl-incf total (file-attribute-size attributes))))))))
      (walk root))
    total))

(defun skg-recovery--iec-size (bytes)
  (let ((value (float bytes))
        (units ["B" "KiB" "MiB" "GiB" "TiB"])
        (unit 0))
    (while (and (>= value 1024.0) (< unit (1- (length units))))
      (setq value (/ value 1024.0))
      (cl-incf unit))
    (if (= unit 0)
        (format "%d B" bytes)
      (format "%.1f %s" value (aref units unit)))))

(defun skg-recovery-archive-size-report (root incident-path)
  "Return exact and IEC sizes for INCIDENT-PATH and retained incidents."
  (let ((incident-bytes (skg-recovery--tree-bytes incident-path))
        (retained-bytes 0)
        (retained-count 0))
    (dolist (path (directory-files
                   root t directory-files-no-dot-files-regexp t))
      (let ((name (file-name-nondirectory path)))
        (when (skg-recovery--strict-final-name-p name)
          (when (file-symlink-p path)
            (skg-recovery--fail
             "strict incident name is a symlink: %s" path))
          (unless (file-directory-p path)
            (skg-recovery--fail
             "strict incident name is not a directory: %s" path))
          (cl-incf retained-count)
          (cl-incf retained-bytes (skg-recovery--tree-bytes path)))))
    (list :incident-bytes incident-bytes
          :incident-iec (skg-recovery--iec-size incident-bytes)
          :retained-bytes retained-bytes
          :retained-iec (skg-recovery--iec-size retained-bytes)
          :retained-count retained-count)))

(cl-defun skg-recovery-archive-publish-initial
    (offer &key buffers archive-root client-nonce undo-waivers)
  "Publish OFFER's initial incident for already locked dirty BUFFERS.
UNDO-WAIVERS is an alist from exact archive buffer key (or buffer ID for
compatibility) to the explicitly approved failure reason.  Return the final
path, manifest checksum, and size report."
  (let* ((incident-id (skg-recovery--offer-value offer 'incident-id))
         (epoch (skg-recovery--offer-value offer 'epoch))
         (archive-name (skg-recovery--offer-value offer 'archive-name)))
    (unless (skg-recovery--strict-uuid-p incident-id)
      (skg-recovery--fail "invalid server-owned incident UUID"))
    (unless (and (integerp epoch) (> epoch 0))
      (skg-recovery--fail "invalid maintenance epoch"))
    (skg-recovery--validate-archive-name archive-name incident-id)
    (unless (and (stringp (skg-recovery--offer-value offer 'origin))
                 (stringp (skg-recovery--offer-value
                           offer 'started-at-utc)))
      (skg-recovery--fail "maintenance offer lacks origin or start time"))
    (let* ((root (skg-recovery-resolve-archive-root archive-root))
           (staging-root (expand-file-name ".staging" root))
           (nonce (or client-nonce (skg-recovery--new-nonce)))
           (staging-name (format "%s.%s.partial" incident-id nonce))
           (staging (expand-file-name staging-name staging-root))
           (final (expand-file-name archive-name root))
           dirty descriptors buffer-records manifest-text manifest-path
           manifest-bytes manifest-sha256 marker-text marker-temp marker-final
           root-artifacts sizes)
      (unless (and (string-match-p "\\`[0-9a-f]+\\'" nonce)
                   (<= 16 (length nonce)) (<= (length nonce) 64))
        (skg-recovery--fail "invalid client staging nonce"))
      (when (file-exists-p final)
        (skg-recovery--fail "final incident path already exists"))
      (skg-recovery--make-private-directory staging-root t)
      (skg-recovery--make-private-directory staging)
      (skg-recovery--make-private-directory
       (expand-file-name "buffer-snapshots" staging))
      (skg-recovery--make-private-directory
       (expand-file-name "interrupted-buffers" staging))
      (dolist (buffer (or buffers (skg-registered-buffers)))
        (when (skg-buffer-dirty-p buffer) (push buffer dirty)))
      (setq descriptors (skg-recovery--allocate-buffer-keys
                         (nreverse dirty)))
      (dolist (descriptor descriptors)
        (let* ((record (plist-get descriptor :record))
               (buffer-id (skg--buffer-record-id record)))
          (unless (equal epoch
                         (skg--buffer-record-maintenance-epoch record))
            (skg-recovery--fail
             "buffer %s is not locked for epoch %d" buffer-id epoch))
          (push
           (skg-recovery--snapshot-one-buffer
            descriptor staging
            (or (cdr (assoc (plist-get descriptor :key) undo-waivers))
                (cdr (assoc buffer-id undo-waivers))))
           buffer-records)))
      (setq buffer-records (nreverse buffer-records))
      (skg-recovery--write-private-file
       (expand-file-name "interrupted-buffers/README.org" staging)
       (skg--utf8-unix-bytes
        "* Interrupted buffers\n\nNo buffer disposition has been selected yet.\n")
       staging)
      (skg-recovery--write-private-file
       (expand-file-name "incident.org" staging)
       (skg--utf8-unix-bytes
        (skg-recovery--incident-readme offer (length descriptors)))
       staging)
      (setq root-artifacts
            (list
             (skg-recovery--artifact-record
              (expand-file-name "incident.org" staging) staging)
             (skg-recovery--artifact-record
              (expand-file-name "interrupted-buffers/README.org" staging)
              staging)))
      (setq manifest-text
            (concat
             (skg-recovery-canonical-sexpr
              (skg-recovery--initial-manifest
               offer root buffer-records root-artifacts))
             "\n")
            manifest-bytes (skg--utf8-unix-bytes manifest-text)
            manifest-path (expand-file-name "manifest.initial.sexp" staging))
      (skg-recovery--write-private-file
       manifest-path manifest-bytes staging)
      (unless (equal (skg-recovery--read-bytes manifest-path)
                     manifest-bytes)
        (skg-recovery--fail "initial manifest changed after durable write"))
      (setq manifest-sha256 (secure-hash 'sha256 manifest-bytes))
      (skg-recovery--sync-filesystem)
      (rename-file staging final nil)
      (skg-recovery--sync-filesystem)
      (setq marker-text
            (skg--utf8-unix-bytes
             (concat
              (skg-recovery-canonical-sexpr
               (skg-recovery--marker-value offer manifest-sha256))
              "\n"))
            marker-temp
            (expand-file-name (format ".ARCHIVE-READY.%s.tmp" nonce) final)
            marker-final (expand-file-name "ARCHIVE-READY" final))
      (skg-recovery--write-private-file marker-temp marker-text final)
      (rename-file marker-temp marker-final nil)
      (skg-recovery--sync-filesystem)
      (unless (and
               (equal (skg-recovery--read-bytes
                       (expand-file-name "manifest.initial.sexp" final))
                      manifest-bytes)
               (equal (skg-recovery--read-bytes marker-final) marker-text))
        (skg-recovery--fail
         "published archive failed final checksum reread"))
      (setq sizes (skg-recovery-archive-size-report root final))
      (message
       (concat
        "Recovery archive ready: %d bytes (%s); "
        "%d retained incident(s), %d bytes (%s) total")
       (plist-get sizes :incident-bytes)
       (plist-get sizes :incident-iec)
       (plist-get sizes :retained-count)
       (plist-get sizes :retained-bytes)
       (plist-get sizes :retained-iec))
      (list :path final
            :archive-name archive-name
            :manifest-sha256 manifest-sha256
            :sizes sizes))))

(provide 'skg-recovery-archive)
