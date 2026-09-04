;;; skg-recovery-archive.el --- Private maintenance recovery archives -*- lexical-binding: t; -*-

;; This module publishes client-owned buffer evidence only.  It never mutates
;; a source, Git repository, or selected server store.  The returned manifest
;; checksum is the exact boundary the server must acknowledge before doing so.

(require 'cl-lib)
(require 'rx)
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

(defconst skg-recovery--evidence-categories
  '("new-nodes" "deleted-nodes" "modified-nodes" "invalid-paths"))

(defun skg-recovery--required-field (alist key context)
  (let ((entry (assoc key alist)))
    (unless (and entry (proper-list-p entry) (= (length entry) 2))
      (skg-recovery--fail "%s lacks exactly one %s field" context key))
    (cadr entry)))

(defun skg-recovery--required-text (alist key context)
  (let ((value (skg-recovery--required-field alist key context)))
    (format "%s" value)))

(defun skg-recovery--required-list (alist key context)
  (let ((value (skg-recovery--required-field alist key context)))
    (unless (proper-list-p value)
      (skg-recovery--fail "%s has a malformed %s list" context key))
    value))

(defun skg-recovery--required-nonnegative-integer (alist key context)
  (let ((value (skg-recovery--required-field alist key context)))
    (unless (and (integerp value) (>= value 0))
      (skg-recovery--fail "%s has an invalid %s" context key))
    value))

(defun skg-recovery--sha256-p (value)
  (and (stringp value)
       (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value)))

(defun skg-recovery--read-exact-sexpr (path)
  (let* ((bytes (skg-recovery--read-bytes path))
         (text (decode-coding-string bytes 'utf-8-unix))
         value end)
    (unless (equal bytes (encode-coding-string text 'utf-8-unix))
      (skg-recovery--fail "machine record is not exact UTF-8: %s" path))
    (condition-case error-data
        (pcase-let ((`(,parsed . ,position) (read-from-string text)))
          (setq value parsed end position))
      (error (skg-recovery--fail "invalid machine record %s: %s"
                                 path (error-message-string error-data))))
    (unless (string-match-p "\\`[[:space:]]*\\'" (substring text end))
      (skg-recovery--fail "machine record has trailing data: %s" path))
    value))

(defun skg-recovery--safe-evidence-path-components (relative)
  (unless (and (stringp relative)
               (not (file-name-absolute-p relative))
               (not (string-match-p "\\\\" relative)))
    (skg-recovery--fail "unsafe evidence path: %S" relative))
  (let ((components (split-string relative "/" nil)))
    (unless (and (>= (length components) 3)
                 (equal relative (string-join components "/"))
                 (member (car components)
                         skg-recovery--evidence-categories)
                 (string-match-p
                  (rx string-start (or "node-" "path-")
                      (= 8 digit) "-" (= 12 (in "0-9a-f")) string-end)
                  (cadr components))
                 (cl-every
                  (lambda (component)
                    (and (not (member component '("" "." "..")))
                         (string-match-p
                          (rx string-start (+ (in "A-Za-z0-9._-"))
                              string-end)
                          component)))
                  components))
      (skg-recovery--fail "unsafe evidence path: %S" relative))
    components))

(defun skg-recovery--parse-evidence-bundle (descriptor opaque-bytes)
  (unless (and (stringp opaque-bytes)
               (not (multibyte-string-p opaque-bytes)))
    (skg-recovery--fail "artifact bundle did not remain opaque bytes"))
  (let* ((context "maintenance evidence descriptor")
         (version (skg-recovery--required-nonnegative-integer
                   descriptor 'artifact-bundle-format-version context))
         (declared-count (skg-recovery--required-nonnegative-integer
                          descriptor 'artifact-count context))
         (declared-bytes (skg-recovery--required-nonnegative-integer
                          descriptor 'artifact-bytes context))
         (payload-sha (skg-recovery--required-text
                       descriptor 'artifact-bytes-sha256 context))
         (transfer-sha (skg-recovery--required-text
                        descriptor 'transfer-manifest-sha256 context))
         (wire-records (skg-recovery--required-list
                        descriptor 'artifacts context))
         (expected-offset 0)
         (seen-keys (make-hash-table :test #'equal))
         (seen-paths (make-hash-table :test #'equal))
         records)
    (unless (= version 1)
      (skg-recovery--fail "unsupported evidence bundle version: %s" version))
    (unless (and (skg-recovery--sha256-p payload-sha)
                 (skg-recovery--sha256-p transfer-sha))
      (skg-recovery--fail "evidence bundle has an invalid checksum"))
    (unless (and (proper-list-p wire-records)
                 (= (length wire-records) declared-count)
                 (= (length opaque-bytes) declared-bytes)
                 (equal (secure-hash 'sha256 opaque-bytes) payload-sha))
      (skg-recovery--fail "opaque evidence inventory/checksum does not match"))
    (cl-loop
     for wire-record in wire-records
     for index from 0
     do
     (let* ((record-context (format "evidence artifact %d" index))
            (key (skg-recovery--required-text
                  wire-record 'artifact-key record-context))
            (relative (skg-recovery--required-text
                       wire-record 'relative-path record-context))
            (purpose (skg-recovery--required-text
                      wire-record 'purpose record-context))
            (offset (skg-recovery--required-nonnegative-integer
                     wire-record 'byte-offset record-context))
            (length (skg-recovery--required-nonnegative-integer
                     wire-record 'byte-length record-context))
            (sha (skg-recovery--required-text
                  wire-record 'sha256 record-context))
            end bytes)
       (unless (equal key (format "artifact-%08d" index))
         (skg-recovery--fail "artifact key/order changed at %d" index))
       (skg-recovery--safe-evidence-path-components relative)
       (unless (and (not (string-empty-p purpose))
                    (skg-recovery--sha256-p sha)
                    (= offset expected-offset)
                    (<= length (- (length opaque-bytes) offset))
                    (not (gethash key seen-keys))
                    (not (gethash relative seen-paths)))
         (skg-recovery--fail "invalid or overlapping %s" record-context))
       (setq end (+ offset length)
             bytes (substring opaque-bytes offset end)
             expected-offset end)
       (unless (equal (secure-hash 'sha256 bytes) sha)
         (skg-recovery--fail "%s checksum mismatch" record-context))
       (puthash key t seen-keys)
       (puthash relative t seen-paths)
       (push (list :key key :relative-path relative :purpose purpose
                   :byte-offset offset :byte-length length :sha256 sha
                   :bytes bytes)
             records)))
    (unless (= expected-offset (length opaque-bytes))
      (skg-recovery--fail "artifact records do not consume the opaque body"))
    (list :records (nreverse records)
          :transfer-manifest-sha256 transfer-sha
          :artifact-bytes-sha256 payload-sha)))

(defun skg-recovery--ensure-private-relative-directory (root relative)
  (let ((cursor root))
    (dolist (component (split-string relative "/" t))
      (setq cursor (expand-file-name component cursor))
      (if (file-exists-p cursor)
          (skg-recovery--require-directory cursor "evidence directory")
        (skg-recovery--make-private-directory cursor)))
    cursor))

(defun skg-recovery--expected-evidence-directories (records category)
  (let ((result (make-hash-table :test #'equal)))
    (puthash category t result)
    (dolist (record records)
      (let* ((relative (plist-get record :relative-path))
             (components (butlast (split-string relative "/" t)))
             accumulated)
        (dolist (component components)
          (setq accumulated (if accumulated
                                (concat accumulated "/" component)
                              component))
          (puthash accumulated t result))))
    result))

(defun skg-recovery--verify-evidence-category
    (incident-root category records)
  (let ((expected-files (make-hash-table :test #'equal))
        (expected-directories
         (skg-recovery--expected-evidence-directories records category))
        (category-root (expand-file-name category incident-root))
        (seen-files 0))
    (dolist (record records)
      (puthash (plist-get record :relative-path) record expected-files))
    (cl-labels
        ((walk
          (directory)
          (skg-recovery--require-directory directory "evidence directory")
          (let ((relative-directory
                 (directory-file-name
                  (file-relative-name directory incident-root))))
            (unless (gethash relative-directory expected-directories)
              (skg-recovery--fail
               "undeclared evidence directory: %s" relative-directory)))
          (dolist (path (directory-files
                         directory t directory-files-no-dot-files-regexp t))
            (let ((attributes (file-attributes path 'integer)))
              (unless attributes
                (skg-recovery--fail "evidence entry vanished: %s" path))
              (pcase (file-attribute-type attributes)
                ((pred stringp)
                 (skg-recovery--fail "evidence entry is a symlink: %s" path))
                ('t (walk path))
                ('nil
                 (unless (and (file-regular-p path)
                              (or (not (file-modes path))
                                  (= (logand (file-modes path) #o777) #o600)))
                   (skg-recovery--fail
                    "evidence entry is not a private regular file: %s" path))
                 (let* ((relative (file-relative-name path incident-root))
                        (record (gethash relative expected-files))
                        (bytes (and record (skg-recovery--read-bytes path))))
                   (unless (and record
                                (= (length bytes)
                                   (plist-get record :byte-length))
                                (equal (secure-hash 'sha256 bytes)
                                       (plist-get record :sha256)))
                     (skg-recovery--fail
                      "undeclared or changed evidence artifact: %s" relative))
                   (cl-incf seen-files)))
                (_ (skg-recovery--fail
                    "special evidence entry is forbidden: %s" path)))))))
      (walk category-root))
    (unless (= seen-files (hash-table-count expected-files))
      (skg-recovery--fail "evidence category %s is incomplete" category))))

(defun skg-recovery--normalize-settlements (settlements initial-buffers)
  (unless (proper-list-p settlements)
    (skg-recovery--fail "view settlements are not a proper list"))
  (let ((seen (make-hash-table :test #'equal))
        normalized)
    (dolist (record settlements)
      (let* ((context "view settlement")
             (buffer-id (skg-recovery--required-text
                         record 'buffer-id context))
             (buffer-key (skg-recovery--required-text
                          record 'buffer-key context))
             (disposition (skg-recovery--required-text
                           record 'planned-disposition context))
             (required-ack (skg-recovery--required-text
                            record 'required-ack context)))
        (when (gethash buffer-id seen)
          (skg-recovery--fail "duplicate settlement for buffer %s" buffer-id))
        (unless (member disposition
                        '("interrupted" "released-unimpacted"
                          "refreshed" "retained-clean" "closed"
                          "detached-derived" "maintenance-aborted" "failed"))
          (skg-recovery--fail "unknown buffer disposition: %s" disposition))
        (unless (member required-ack
                        '("retirement-ack" "release-ack"
                          "application-ack" "close-ack"))
          (skg-recovery--fail "unknown settlement acknowledgement: %s"
                              required-ack))
        (puthash buffer-id buffer-key seen)
        (push
         (list
          (skg-recovery--field 'buffer-id buffer-id)
          (skg-recovery--field 'buffer-key buffer-key)
          (skg-recovery--field
           'kind (skg-recovery--required-text record 'kind context))
          (skg-recovery--field
           'view-uri (skg-recovery--required-text record 'view-uri context))
          (skg-recovery--field
           'dirty (skg-recovery--required-text record 'dirty context))
          (skg-recovery--field
           'impacted (skg-recovery--required-text record 'impacted context))
          (skg-recovery--field
           'parse-uncertain
           (skg-recovery--required-text record 'parse-uncertain context))
          (skg-recovery--field
           'observed-ids
           (mapcar (lambda (value) (format "%s" value))
                   (skg-recovery--required-list
                    record 'observed-ids context)))
          (skg-recovery--field
           'resolved-primary-ids
           (mapcar (lambda (value) (format "%s" value))
                   (skg-recovery--required-list
                    record 'resolved-primary-ids context)))
          (skg-recovery--field
           'base-server-revision
           (skg-recovery--required-nonnegative-integer
            record 'base-server-revision context))
          (skg-recovery--field
           'base-application-token
           (skg-recovery--required-nonnegative-integer
            record 'base-application-token context))
          (skg-recovery--field 'disposition disposition)
          (skg-recovery--field 'required-ack required-ack)
          (skg-recovery--field 'acknowledged "true"))
         normalized)))
    (dolist (buffer initial-buffers)
      (let ((buffer-id (skg-recovery--required-text
                        buffer 'buffer-id "initial buffer"))
            (buffer-key (skg-recovery--required-text
                         buffer 'buffer-key "initial buffer")))
        (unless (equal (gethash buffer-id seen) buffer-key)
          (skg-recovery--fail
           "dirty buffer %s has no exact final settlement" buffer-id))))
    (nreverse normalized)))

(defun skg-recovery--final-artifact-record (record)
  (list
   (skg-recovery--field 'artifact-key (plist-get record :key))
   (skg-recovery--field 'path (plist-get record :relative-path))
   (skg-recovery--field 'purpose (plist-get record :purpose))
   (skg-recovery--field 'byte-offset (plist-get record :byte-offset))
   (skg-recovery--field 'bytes (plist-get record :byte-length))
   (skg-recovery--field 'sha256 (plist-get record :sha256))))

(defun skg-recovery--final-manifest
    (initial initial-sha descriptor evidence settlements root-artifacts)
  (let ((context "maintenance evidence descriptor"))
    (list
     (skg-recovery--field 'archive-format-version
                          skg-recovery-archive-format-version)
     (skg-recovery--field 'manifest-kind "final")
     (skg-recovery--field
      'incident-id (skg-recovery--required-text descriptor 'incident-id context))
     (skg-recovery--field
      'maintenance-epoch
      (skg-recovery--required-nonnegative-integer
       descriptor 'maintenance-epoch context))
     (skg-recovery--field 'initial-manifest-sha256 initial-sha)
     (skg-recovery--field
      'origin (skg-recovery--required-text initial 'origin "initial manifest"))
     (skg-recovery--field
      'started-at-utc
      (skg-recovery--required-text initial 'started-at-utc "initial manifest"))
     (skg-recovery--field
      'client-kind
      (skg-recovery--required-text initial 'client-kind "initial manifest"))
     (skg-recovery--field
      'client-version
      (skg-recovery--required-text initial 'client-version "initial manifest"))
     (skg-recovery--field
      'candidate-id
      (skg-recovery--required-text descriptor 'candidate-id context))
     (skg-recovery--field
      'g0-graph-generation
      (skg-recovery--required-nonnegative-integer
       descriptor 'g0-graph-generation context))
     (skg-recovery--field
      'g0-manifest-revision
      (skg-recovery--required-nonnegative-integer
       descriptor 'g0-manifest-revision context))
     (skg-recovery--field
      'g1-graph-generation
      (skg-recovery--required-nonnegative-integer
       descriptor 'g1-graph-generation context))
     (skg-recovery--field
      'g1-manifest-revision
      (skg-recovery--required-nonnegative-integer
       descriptor 'g1-manifest-revision context))
     (skg-recovery--field
      'tantivy-generation
      (skg-recovery--required-nonnegative-integer
       descriptor 'tantivy-generation context))
     (skg-recovery--field
      'server-evidence-sha256
      (skg-recovery--required-text descriptor 'server-evidence-sha256 context))
     (skg-recovery--field
      'transfer-manifest-sha256
      (plist-get evidence :transfer-manifest-sha256))
     (skg-recovery--field
      'artifact-bytes-sha256 (plist-get evidence :artifact-bytes-sha256))
     (skg-recovery--field
      'node-artifacts
      (mapcar #'skg-recovery--final-artifact-record
              (plist-get evidence :records)))
     (skg-recovery--field 'root-artifacts root-artifacts)
     (skg-recovery--field 'buffers
                          (skg-recovery--required-list
                           initial 'buffers "initial manifest"))
     (skg-recovery--field 'buffer-dispositions settlements)
     (skg-recovery--field 'directory-sync
                          (if (fboundp 'unix-sync)
                              "unix-sync" "unavailable"))
     (skg-recovery--field 'terminal-status "completed"))))

(defun skg-recovery--final-incident-report
    (descriptor evidence settlements)
  (concat
   "* Skg maintenance recovery incident (finalized)\n\n"
   (format "- Incident :: =%s=\n"
           (skg-recovery--required-text
            descriptor 'incident-id "evidence descriptor"))
   (format "- Candidate :: =%s=\n"
           (skg-recovery--required-text
            descriptor 'candidate-id "evidence descriptor"))
   (format "- Selected graph :: =%s=\n"
           (skg-recovery--required-nonnegative-integer
            descriptor 'g1-graph-generation "evidence descriptor"))
   (format "- Node evidence artifacts :: %d\n\n"
           (length (plist-get evidence :records)))
   "** Buffer dispositions\n\n"
   (if settlements
       (mapconcat
        (lambda (record)
          (format "- =%s= :: %s\n"
                  (skg-recovery--required-text
                   record 'buffer-id "final settlement")
                  (skg-recovery--required-text
                   record 'disposition "final settlement")))
        settlements "")
     "No buffers were registered.\n")))

(defun skg-recovery--interrupted-index (settlements)
  (concat
   "* Interrupted buffers\n\n"
   (let (links)
     (dolist (record settlements)
       (when (equal (skg-recovery--required-text
                     record 'disposition "final settlement")
                    "interrupted")
         (let ((key (skg-recovery--required-text
                     record 'buffer-key "final settlement")))
           (unless (string-match-p
                    (rx string-start alnum
                        (* (in "A-Za-z0-9._-")) string-end)
                    key)
             (skg-recovery--fail "unsafe interrupted buffer key: %s" key))
           (push (format "- [[file:../buffer-snapshots/%s/unsaved-changes.org][%s]]\n"
                         key key)
                 links))))
     (if links (apply #'concat (nreverse links))
       "No dirty buffer was interrupted.\n"))))

(defun skg-recovery--replace-private-file (path bytes incident-root token)
  (let ((temporary (expand-file-name
                    (format ".%s.%s.tmp" (file-name-nondirectory path) token)
                    (file-name-directory path))))
    (skg-recovery--write-private-file temporary bytes incident-root)
    (rename-file temporary path t)
    (unless (equal (skg-recovery--read-bytes path) bytes)
      (skg-recovery--fail "atomic replacement failed exact reread: %s" path))))

(defun skg-recovery--finalized-marker (incident-id manifest-sha transfer-sha)
  (list
   (skg-recovery--field 'archive-format-version
                        skg-recovery-archive-format-version)
   (skg-recovery--field 'incident-id incident-id)
   (skg-recovery--field 'manifest-sha256 manifest-sha)
   (skg-recovery--field 'transfer-manifest-sha256 transfer-sha)))

(defun skg-recovery--bytes-artifact-record (relative bytes)
  (list
   (skg-recovery--field 'path relative)
   (skg-recovery--field 'bytes (length bytes))
   (skg-recovery--field 'sha256 (secure-hash 'sha256 bytes))))

(cl-defun skg-recovery-archive-finalize
    (initial-result descriptor opaque-bytes settlements)
  "Append exact server evidence and final dispositions to INITIAL-RESULT.
DESCRIPTOR is the parsed UTF-8 artifact descriptor and OPAQUE-BYTES is the
unibyte tail delivered with it.  SETTLEMENTS are the exact records already
applied and acknowledged by the editor.  The operation is replay-safe and
creates FINALIZED only after every other durable artifact."
  (let* ((incident-root (plist-get initial-result :path))
         (initial-sha (plist-get initial-result :manifest-sha256))
         (initial-path (and incident-root
                            (expand-file-name "manifest.initial.sexp"
                                              incident-root)))
         (ready-path (and incident-root
                          (expand-file-name "ARCHIVE-READY" incident-root))))
    (unless (and (stringp incident-root) (stringp initial-sha)
                 (skg-recovery--sha256-p initial-sha))
      (skg-recovery--fail "initial archive result is incomplete"))
    (skg-recovery--require-directory incident-root "incident directory")
    (let* ((initial-bytes (skg-recovery--read-bytes initial-path))
           (initial (skg-recovery--read-exact-sexpr initial-path))
           (ready (skg-recovery--read-exact-sexpr ready-path))
           (incident-id (skg-recovery--required-text
                         descriptor 'incident-id "evidence descriptor"))
           (epoch (skg-recovery--required-nonnegative-integer
                   descriptor 'maintenance-epoch "evidence descriptor"))
           (evidence (skg-recovery--parse-evidence-bundle
                      descriptor opaque-bytes))
           (normalized-settlements
            (skg-recovery--normalize-settlements
             settlements
             (skg-recovery--required-list
              initial 'buffers "initial manifest")))
           (transfer-sha (plist-get evidence :transfer-manifest-sha256))
           (token (substring transfer-sha 0 20))
           (attempt-nonce (skg-recovery--new-nonce))
           (attempt-token (format "%s-%s" token attempt-nonce))
           (staging (expand-file-name
                     (format ".finalizing.%s.%s.partial"
                             token attempt-nonce)
                     incident-root))
           (incident-report
            (skg--utf8-unix-bytes
             (skg-recovery--final-incident-report
              descriptor evidence normalized-settlements)))
           (interrupted-index
            (skg--utf8-unix-bytes
             (skg-recovery--interrupted-index normalized-settlements)))
           (root-artifacts
            (list
             (skg-recovery--bytes-artifact-record
              "incident.org" incident-report)
             (skg-recovery--bytes-artifact-record
              "interrupted-buffers/README.org" interrupted-index)))
           (final-value
            (skg-recovery--final-manifest
             initial initial-sha descriptor evidence normalized-settlements
             root-artifacts))
           (final-bytes
            (skg--utf8-unix-bytes
             (concat (skg-recovery-canonical-sexpr final-value) "\n")))
           (final-sha (secure-hash 'sha256 final-bytes))
           (marker-bytes
            (skg--utf8-unix-bytes
             (concat
              (skg-recovery-canonical-sexpr
               (skg-recovery--finalized-marker
                incident-id final-sha transfer-sha))
              "\n")))
           (final-manifest-path
            (expand-file-name "manifest.final.sexp" incident-root))
           (finalized-path (expand-file-name "FINALIZED" incident-root)))
      (unless (and (equal (secure-hash 'sha256 initial-bytes) initial-sha)
                   (skg-recovery--strict-uuid-p incident-id)
                   (skg-recovery--strict-uuid-p
                    (skg-recovery--required-text
                     descriptor 'candidate-id "evidence descriptor"))
                   (equal (skg-recovery--required-text
                           initial 'manifest-kind "initial manifest")
                          "initial")
                   (equal (skg-recovery--required-text
                           initial 'incident-id "initial manifest")
                          incident-id)
                   (= (skg-recovery--required-nonnegative-integer
                       initial 'maintenance-epoch "initial manifest") epoch)
                   (= (skg-recovery--required-nonnegative-integer
                       initial 'g0-graph-generation "initial manifest")
                      (skg-recovery--required-nonnegative-integer
                       descriptor 'g0-graph-generation "evidence descriptor"))
                   (= (skg-recovery--required-nonnegative-integer
                       initial 'g0-manifest-revision "initial manifest")
                      (skg-recovery--required-nonnegative-integer
                       descriptor 'g0-manifest-revision "evidence descriptor"))
                   (equal (skg-recovery--required-text
                           ready 'incident-id "ARCHIVE-READY") incident-id)
                   (equal (skg-recovery--required-text
                           ready 'manifest-sha256 "ARCHIVE-READY") initial-sha))
        (skg-recovery--fail
         "evidence does not belong to this exact ready archive"))
      (dolist (key '(server-evidence-sha256 transfer-manifest-sha256
                     artifact-bytes-sha256))
        (unless (skg-recovery--sha256-p
                 (skg-recovery--required-text descriptor key
                                               "evidence descriptor"))
          (skg-recovery--fail "evidence descriptor has an invalid %s" key)))

      ;; Install whole top-level evidence categories.  A prior completed
      ;; rename is accepted only after its exact closed inventory is proved.
      (skg-recovery--make-private-directory staging)
      (dolist (category skg-recovery--evidence-categories)
        (let* ((records
                (cl-remove-if-not
                 (lambda (record)
                   (equal (car (skg-recovery--safe-evidence-path-components
                                (plist-get record :relative-path)))
                          category))
                 (plist-get evidence :records)))
               (destination (expand-file-name category incident-root)))
          (when records
            (if (file-exists-p destination)
                (skg-recovery--verify-evidence-category
                 incident-root category records)
              (skg-recovery--make-private-directory
               (expand-file-name category staging))
              (dolist (record records)
                (let* ((relative (plist-get record :relative-path))
                       (parent (directory-file-name
                                (file-name-directory relative))))
                  (skg-recovery--ensure-private-relative-directory
                   staging parent)
                  (skg-recovery--write-private-file
                   (expand-file-name relative staging)
                   (plist-get record :bytes) staging)))
              (skg-recovery--verify-evidence-category staging category records)
              (skg-recovery--sync-filesystem)
              (rename-file (expand-file-name category staging)
                           destination nil)
              (skg-recovery--sync-filesystem)
              (skg-recovery--verify-evidence-category
               incident-root category records)))
          (when (and (null records) (file-exists-p destination))
            (skg-recovery--fail
             "archive contains undeclared evidence category: %s" category))))
      (when (null (directory-files
                   staging nil directory-files-no-dot-files-regexp t))
        (delete-directory staging))

      (if (file-exists-p finalized-path)
          (unless (and (equal (skg-recovery--read-bytes final-manifest-path)
                              final-bytes)
                       (equal (skg-recovery--read-bytes finalized-path)
                              marker-bytes)
                       (equal (skg-recovery--read-bytes
                               (expand-file-name "incident.org" incident-root))
                              incident-report)
                       (equal (skg-recovery--read-bytes
                               (expand-file-name
                                "interrupted-buffers/README.org"
                                incident-root))
                              interrupted-index))
            (skg-recovery--fail
             "existing FINALIZED archive differs from this exact replay"))
        (skg-recovery--replace-private-file
         (expand-file-name "incident.org" incident-root)
         incident-report incident-root attempt-token)
        (skg-recovery--replace-private-file
         (expand-file-name "interrupted-buffers/README.org" incident-root)
         interrupted-index incident-root attempt-token)
        (if (file-exists-p final-manifest-path)
            (unless (equal (skg-recovery--read-bytes final-manifest-path)
                           final-bytes)
              (skg-recovery--fail
               "existing final manifest differs from exact replay"))
          (skg-recovery--replace-private-file
           final-manifest-path final-bytes incident-root attempt-token))
        (skg-recovery--sync-filesystem)
        (let ((temporary (expand-file-name
                          (format ".FINALIZED.%s.tmp" attempt-token)
                          incident-root)))
          (skg-recovery--write-private-file temporary marker-bytes incident-root)
          (rename-file temporary finalized-path nil))
        (skg-recovery--sync-filesystem))
      (unless (and (equal (skg-recovery--read-bytes final-manifest-path)
                          final-bytes)
                   (equal (skg-recovery--read-bytes finalized-path)
                          marker-bytes))
        (skg-recovery--fail "final archive failed durable checksum reread"))
      (let ((sizes (skg-recovery-archive-size-report
                    (file-name-directory incident-root) incident-root)))
        (list :path incident-root
              :manifest-sha256 final-sha
              :transfer-manifest-sha256 transfer-sha
              :artifact-bytes-sha256
              (plist-get evidence :artifact-bytes-sha256)
              :sizes sizes)))))

(provide 'skg-recovery-archive)
