;;; skg-undo-sidecar.el --- Native Emacs undo archive adapter -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer-registry)

(define-error 'skg-undo-sidecar-error "Skg native undo archive failed")

;; Declare the package's documented dynamic options without loading or enabling
;; it.  `require' below supplies their defaults; every archive call binds all
;; values on which it relies.
(defvar undo-fu-session-directory)
(defvar undo-fu-session-compression)
(defvar undo-fu-session-linear)
(defvar undo-fu-session-file-limit)
(defvar undo-fu-session-ignore-encrypted-files)
(defvar undo-fu-session-ignore-temp-files)
(defvar undo-fu-session-incompatible-files)
(defvar undo-fu-session-incompatible-major-modes)
(defvar undo-fu-session-make-file-name-function)
(defvar undo-fu-session-mode)

(defun skg-undo-sidecar-package-version ()
  "Return the installed undo-fu-session version, without enabling a mode."
  (when-let ((library (locate-library "undo-fu-session")))
    (require 'lisp-mnt)
    ;; Package installation byte-compiles the library. The compiled file has
    ;; no Version header; inspect its packaged source without enabling a mode.
    (let ((source (if (string-suffix-p ".elc" library)
                      (substring library 0 -1)
                    library)))
      (when (file-readable-p source)
        (with-temp-buffer
          (insert-file-contents source)
          (lm-header "version"))))))

(defun skg-undo-sidecar-capability-error ()
  "Return nil when native undo archiving is supported, else a reason."
  (cond
   ((bound-and-true-p undo-tree-mode)
    "undo-tree-mode is active; undo-fu-session cannot serialize its history")
   ((not (equal (skg-undo-sidecar-package-version) "0.8"))
    "undo-fu-session 0.8 is not installed")
   (t nil)))

(defun skg-undo-sidecar--history-present-p ()
  (cond
   ((eq buffer-undo-list t)
    (signal 'skg-undo-sidecar-error '("undo is disabled in this buffer")))
   ((or (consp buffer-undo-list)
        (consp pending-undo-list)) t)
   (t nil)))

(defmacro skg-undo-sidecar--with-adapter
    (pseudo-file sidecar package-directory &rest body)
  "Run BODY through the public 0.8 API, confined below the incident."
  (declare (indent 3))
  `(let* ((expected-pseudo (expand-file-name ,pseudo-file))
          (expected-sidecar (expand-file-name ,sidecar))
          (confined-directory (file-name-as-directory
                               (expand-file-name ,package-directory)))
          (undo-fu-session-directory confined-directory)
          (undo-fu-session-compression 'gz)
          (undo-fu-session-linear nil)
          (undo-fu-session-file-limit nil)
          (undo-fu-session-ignore-encrypted-files nil)
          (undo-fu-session-ignore-temp-files nil)
          (undo-fu-session-incompatible-files nil)
          (undo-fu-session-incompatible-major-modes nil)
          (undo-fu-session-make-file-name-function
           (lambda (filepath extension)
             (unless (equal (expand-file-name filepath) expected-pseudo)
               (error "undo adapter received an unexpected pseudo-file"))
             (unless (equal extension ".gz")
               (error "undo adapter received an unexpected extension"))
             expected-sidecar))
          ;; Binding the public minor-mode variable is sufficient for the
          ;; wrappers.  Invoking the mode would install unrelated file hooks.
          (undo-fu-session-mode t)
          (buffer-file-name expected-pseudo)
          (buffer-file-coding-system 'utf-8-unix))
     (with-file-modes #o600
       ,@body)))

(defun skg-undo-sidecar--assert-confined (path directory)
  (let ((path (expand-file-name path))
        (directory (file-name-as-directory (expand-file-name directory))))
    (unless (string-prefix-p directory path)
      (signal 'skg-undo-sidecar-error
              (list (format "undo path escapes incident staging: %s" path))))))

(defun skg-undo-sidecar--read-bytes (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun skg-undo-sidecar--sha256-file (path)
  (secure-hash 'sha256 (skg-undo-sidecar--read-bytes path)))

(defun skg-undo-sidecar--raw-text ()
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun skg-undo-sidecar--validate-objects (tree buffer)
  (cl-labels
      ((walk
        (value)
        (cond
         ((markerp value)
          (unless (eq (marker-buffer value) buffer)
            (signal 'skg-undo-sidecar-error
                    '("recovered undo marker belongs to another buffer"))))
         ((overlayp value)
          (unless (or (null (overlay-buffer value))
                      (eq (overlay-buffer value) buffer))
            (signal 'skg-undo-sidecar-error
                    '("recovered undo overlay belongs to another buffer"))))
         ((consp value) (walk (car value)) (walk (cdr value)))
         ((vectorp value) (mapc #'walk value)))))
    (walk tree)))

(defun skg-undo-sidecar--next-group-has-apply-p ()
  "Return non-nil if the next real undo group contains an apply record."
  (let ((tail buffer-undo-list))
    (while (and tail (null (car tail))) (setq tail (cdr tail)))
    (catch 'found
      (while (and (consp tail) (car tail))
        (when (and (consp (car tail))
                   (eq (caar tail) 'apply))
          (throw 'found t))
        (setq tail (cdr tail)))
      nil)))

(defun skg-undo-sidecar--validate-recovered-state (buffer)
  (with-current-buffer buffer
    (unless (or (null buffer-undo-list) (listp buffer-undo-list))
      (signal 'skg-undo-sidecar-error '("invalid recovered buffer-undo-list")))
    (unless (or (null pending-undo-list)
                (listp pending-undo-list)
                (eq pending-undo-list t))
      (signal 'skg-undo-sidecar-error '("invalid recovered pending undo")))
    (skg-undo-sidecar--validate-objects buffer-undo-list buffer)
    (skg-undo-sidecar--validate-objects pending-undo-list buffer)
    (if (skg-undo-sidecar--next-group-has-apply-p)
        'structurally-validated-apply
      (let ((before (skg-undo-sidecar--raw-text))
            (last-command nil))
        (condition-case err
            (progn
              (let ((this-command 'undo)) (undo-only 1))
              (setq last-command 'undo)
              (let ((this-command 'undo-redo)) (undo-redo 1))
              (unless (equal before (skg-undo-sidecar--raw-text))
                (signal 'skg-undo-sidecar-error
                        '("undo/redo probe did not restore exact text")))
              'undo-redo-probed)
          (error
           (signal 'skg-undo-sidecar-error
                   (list (format "undo/redo probe failed: %s"
                                 (error-message-string err))))))))))

(defun skg-undo-sidecar--recover (buffer pseudo-file sidecar package-dir)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (skg-undo-sidecar--with-adapter pseudo-file sidecar package-dir
        (unless (undo-fu-session-recover)
          (signal 'skg-undo-sidecar-error
                  '("undo-fu-session refused the archived text"))))))
  (skg-undo-sidecar--validate-recovered-state buffer))

(defun skg-undo-sidecar--require-private-regular (path description)
  (let ((attributes (file-attributes path 'integer)))
    (when (file-symlink-p path)
      (signal 'skg-undo-sidecar-error
              (list (format "%s is a symlink" description))))
    (unless (and attributes
                 (file-regular-p path)
                 (= (logand (or (file-modes path) 0) #o777) #o600)
                 (= (file-attribute-link-number attributes) 1))
      (signal 'skg-undo-sidecar-error
              (list (format "%s is not a private regular file"
                            description))))))

(defun skg-undo-sidecar-restore
    (buffer pseudo-file sidecar &optional expected-version)
  "Restore SIDECAR into BUFFER without writing below the archive.
PSEUDO-FILE contains BUFFER's exact archived text.  EXPECTED-VERSION, when
non-nil, must name the exact supported undo-fu-session version."
  (with-current-buffer buffer
    (when (and expected-version (not (equal expected-version "0.8")))
      (signal 'skg-undo-sidecar-error
              (list (format "unsupported archived undo version %s"
                            expected-version))))
    (when-let ((reason (skg-undo-sidecar-capability-error)))
      (signal 'skg-undo-sidecar-error (list reason)))
    (skg-undo-sidecar--require-private-regular
     pseudo-file "current-text artifact")
    (skg-undo-sidecar--require-private-regular
     sidecar "native undo sidecar")
    (let* ((pseudo-bytes (skg-undo-sidecar--read-bytes pseudo-file))
           (sidecar-bytes (skg-undo-sidecar--read-bytes sidecar))
           (live-bytes (skg--utf8-unix-bytes
                        (skg-undo-sidecar--raw-text)))
           (package-dir (make-temp-file "skg-undo-restore-" t))
           validation)
      (unless (equal live-bytes pseudo-bytes)
        (signal 'skg-undo-sidecar-error
                '("recovery buffer text differs from its archive artifact")))
      (set-file-modes package-dir #o700)
      (require 'undo-fu-session)
      (unwind-protect
          (setq validation
                (skg-undo-sidecar--recover
                 buffer pseudo-file sidecar package-dir))
        (when (file-directory-p package-dir)
          (delete-directory package-dir t)))
      (unless (and (equal pseudo-bytes
                          (skg-undo-sidecar--read-bytes pseudo-file))
                   (equal sidecar-bytes
                          (skg-undo-sidecar--read-bytes sidecar))
                   (equal live-bytes
                          (skg--utf8-unix-bytes
                           (skg-undo-sidecar--raw-text))))
        (signal 'skg-undo-sidecar-error
                '("native undo restore changed archived or live text")))
      validation)))

(defun skg-undo-sidecar-save (buffer pseudo-file sidecar incident-staging)
  "Archive BUFFER's native history to SIDECAR and verify a round trip.
PSEUDO-FILE is the already-written exact current-text artifact.  Return a
portable status plist; signal `skg-undo-sidecar-error' on required failure."
  (catch 'skg-undo-sidecar-result
    (dolist (path (list pseudo-file sidecar))
      (skg-undo-sidecar--assert-confined path incident-staging))
    (let ((package-dir
         (expand-file-name ".undo-fu-session-confined"
                           (file-name-directory sidecar))))
    (skg-undo-sidecar--assert-confined package-dir incident-staging)
    (with-current-buffer buffer
      (unless (equal (secure-hash 'sha256 (skg--utf8-unix-bytes
                                          (skg-undo-sidecar--raw-text)))
                     (skg-undo-sidecar--sha256-file pseudo-file))
        (signal 'skg-undo-sidecar-error
                '("current buffer text differs from its archive artifact")))
      (unless (skg-undo-sidecar--history-present-p)
        (throw 'skg-undo-sidecar-result
          `(:status empty :kind undo-fu-session
            :version ,(or (skg-undo-sidecar-package-version)
                          "not-required"))))
      (when-let ((reason (skg-undo-sidecar-capability-error)))
        (signal 'skg-undo-sidecar-error (list reason)))
      (require 'undo-fu-session)
      (with-file-modes #o700
        (make-directory package-dir t))
      (set-file-modes package-dir #o700)
      (when (file-exists-p sidecar)
        (signal 'skg-undo-sidecar-error '("undo sidecar already exists")))
      (save-restriction
        (widen)
        (skg-undo-sidecar--with-adapter pseudo-file sidecar package-dir
          ;; The public save wrapper intentionally returns nil on success.
          (undo-fu-session-save))))
    (when (file-symlink-p sidecar)
      (signal 'skg-undo-sidecar-error '("undo sidecar is a symlink")))
    (unless (and (file-regular-p sidecar)
                 (> (file-attribute-size (file-attributes sidecar)) 0))
      (signal 'skg-undo-sidecar-error
              '("undo-fu-session produced no regular sidecar")))
    (set-file-modes sidecar #o600)
    (when (directory-files package-dir nil directory-files-no-dot-files-regexp)
      (signal 'skg-undo-sidecar-error
              '("undo-fu-session produced unexpected fallback output")))
    (delete-directory package-dir)
    (let* ((current-bytes (skg-undo-sidecar--read-bytes pseudo-file))
           (current-text (decode-coding-string current-bytes 'utf-8-unix t))
           (probe (generate-new-buffer " *skg-undo-round-trip*"))
           (verify-dir
            (expand-file-name ".undo-fu-session-verify"
                              (file-name-directory sidecar)))
           validation)
      (unwind-protect
          (progn
            (with-current-buffer probe
              (org-mode)
              (buffer-disable-undo)
              (insert current-text)
              (buffer-enable-undo)
              (setq buffer-undo-list nil pending-undo-list nil)
              (set-buffer-modified-p nil))
            (setq validation
                  (skg-undo-sidecar--recover
                   probe pseudo-file sidecar
                   verify-dir)))
        (when (buffer-live-p probe)
          (with-current-buffer probe (set-buffer-modified-p nil))
          (kill-buffer probe)))
      (when (and (file-directory-p verify-dir)
                 (null (directory-files
                        verify-dir nil directory-files-no-dot-files-regexp)))
        (delete-directory verify-dir))
      `(:status archived
        :kind undo-fu-session
        :version "0.8"
        :compression gz
        :validation ,validation
        :bytes ,(file-attribute-size (file-attributes sidecar))
        :sha256 ,(skg-undo-sidecar--sha256-file sidecar)
        :path ,sidecar)))))

(provide 'skg-undo-sidecar)
