;;; skg-recovery-ui.el --- Detached maintenance recovery UI -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'tabulated-list)
(require 'subr-x)
(require 'skg-recovery-archive)
(require 'skg-request-single-root-content-view)
(require 'skg-request-text-search)
(require 'skg-undo-sidecar)

(defvar skg--maintenance-client-incident)

(defvar skg-recovery-ui-incident-picker
  #'skg-recovery-ui--default-incident-picker
  "Function which selects one summary from a list of incident summaries.")

(defvar skg-recovery-ui-buffer-picker
  #'skg-recovery-ui--default-buffer-picker
  "Function which selects one archived buffer record from a list.")

(defvar skg-recovery-ui-root-picker
  #'skg-recovery-ui--default-root-picker
  "Function which selects one recorded root ID from a list.")

(defvar-local skg-recovery-incident nil)
(defvar-local skg-recovery-buffer-record nil)
(defvar-local skg-recovery-metadata nil)
(defvar-local skg-recovery-artifact-links nil)
(defvar-local skg-recovery-native-undo-status nil)

(defun skg-recovery-refuse-write (&rest _)
  (user-error
   "This is detached recovery text; copy into a live view or ordinary buffer"))

(defun skg-recovery-undo (&optional count)
  "Undo in detached recovery without invoking Skg."
  (interactive "p")
  (unless (derived-mode-p 'skg-recovery-mode)
    (user-error "This is not a detached recovery buffer"))
  (undo-only (or count 1)))

(defun skg-recovery-redo (&optional count)
  "Redo in detached recovery without invoking Skg."
  (interactive "p")
  (unless (derived-mode-p 'skg-recovery-mode)
    (user-error "This is not a detached recovery buffer"))
  (undo-redo (or count 1)))

(defvar skg-recovery-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map [remap save-buffer] #'skg-recovery-refuse-write)
    (define-key map [remap write-file] #'skg-recovery-refuse-write)
    (define-key map (kbd "C-c C-o") #'skg-recovery-open-artifact)
    (define-key map (kbd "C-c C-f")
                #'skg-open-fresh-view-for-interrupted)
    (define-key map (kbd "C-/") #'skg-recovery-undo)
    (define-key map (kbd "C-?") #'skg-recovery-redo)
    map)
  "Keymap for `skg-recovery-mode'.")

(define-derived-mode skg-recovery-mode org-mode "SKG-Recovery"
  "Major mode for an independent copy of archived Skg buffer text."
  (setq-local org-adapt-indentation nil)
  (setq-local buffer-offer-save nil)
  (setq-local write-contents-functions '(skg-recovery-refuse-write))
  (setq-local write-file-functions '(skg-recovery-refuse-write))
  (setq-local mode-line-process '((:eval " Recovery")))
  (setq-local header-line-format
              "Detached recovery — C-c C-o: artifacts; C-c C-f: fresh live view")
  (setq buffer-file-name nil)
  (auto-save-mode -1))

(defvar-local skg-recovery-list-summaries nil)

(defvar skg-recovery-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'skg-open-interrupted-view)
    (define-key map (kbd "f") #'skg-open-fresh-view-for-interrupted)
    (define-key map (kbd "d") #'skg-delete-maintenance-incident)
    (define-key map (kbd "g") #'skg-list-maintenance-incidents)
    map)
  "Keymap for `skg-recovery-list-mode'.")

(define-derived-mode skg-recovery-list-mode tabulated-list-mode
  "SKG-Incidents"
  "Mode for retained maintenance incident summaries."
  (setq tabulated-list-format
        [("Time (UTC)" 27 t)
         ("Origin" 25 t)
         ("Status" 14 t)
         ("Nodes" 7 t)
         ("Interrupted" 11 t)
         ("Released" 8 t)
         ("Client" 10 t)
         ("G0/G1" 12 t)
         ("Bytes" 14 t)
         ("Undo" 6 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Time (UTC)" . t))
  (tabulated-list-init-header))

(defun skg-recovery-ui--summary-row (summary)
  (let ((invalid (eq (plist-get summary :status) 'invalid)))
    (list
     (plist-get summary :path)
     (vector
      (or (plist-get summary :started-at-utc) "")
      (or (plist-get summary :origin) "")
      (symbol-name (plist-get summary :status))
      (if invalid "" (number-to-string
                        (plist-get summary :changed-nodes)))
      (if invalid "" (number-to-string
                        (plist-get summary :interrupted-buffers)))
      (if invalid "" (number-to-string
                        (plist-get summary :released-buffers)))
      (or (plist-get summary :client-kind) "")
      (if invalid ""
        (format "%s/%s" (plist-get summary :g0)
                (or (plist-get summary :g1) "—")))
      (if invalid "" (format "%d (%s)"
                               (plist-get summary :bytes)
                               (plist-get summary :iec)))
      (if (and (not invalid)
               (plist-get summary :native-undo-compatible))
          "yes" "no")))))

(defun skg-list-maintenance-incidents ()
  "Show every retained maintenance incident without contacting the server."
  (interactive)
  (let* ((summaries (skg-recovery-archive-list))
         (buffer (get-buffer-create "*Skg maintenance incidents*")))
    (with-current-buffer buffer
      (skg-recovery-list-mode)
      (setq skg-recovery-list-summaries summaries)
      (setq tabulated-list-entries
            (mapcar #'skg-recovery-ui--summary-row summaries))
      (tabulated-list-print t))
    (pop-to-buffer buffer)
    buffer))

(defun skg-recovery-ui--default-incident-picker (summaries prompt)
  (unless summaries (user-error "No retained maintenance incidents"))
  (let* ((choices
          (mapcar
           (lambda (summary)
             (cons
              (format "%s  %-24s  %s"
                      (or (plist-get summary :started-at-utc)
                          (plist-get summary :name))
                      (or (plist-get summary :origin) "invalid")
                      (plist-get summary :status))
              summary))
           summaries))
         (choice (completing-read prompt choices nil t)))
    (cdr (assoc choice choices))))

(defun skg-recovery-ui--default-buffer-picker (records prompt)
  (unless records (user-error "This incident has no interrupted buffers"))
  (let* ((choices
          (mapcar
           (lambda (record)
             (cons (format "%s  [%s]  %s"
                           (skg-recovery--required-text
                            record 'buffer-key "initial buffer")
                           (skg-recovery--required-text
                            record 'kind "initial buffer")
                           (skg-recovery--required-text
                            record 'name "initial buffer"))
                   record))
           records))
         (choice (completing-read prompt choices nil t)))
    (cdr (assoc choice choices))))

(defun skg-recovery-ui--default-root-picker (roots prompt)
  (completing-read prompt roots nil t))

(defun skg-recovery-ui--incident-at-point ()
  (when (derived-mode-p 'skg-recovery-list-mode)
    (when-let ((path (tabulated-list-get-id)))
      (skg-recovery-archive-inspect path))))

(defun skg-recovery-ui--resolve-incident (&optional incident)
  (cond
   ((and (listp incident) (plist-get incident :path))
    (skg-recovery-archive-inspect (plist-get incident :path)))
   ((stringp incident) (skg-recovery-archive-inspect incident))
   ((and (derived-mode-p 'skg-recovery-mode) skg-recovery-incident)
    (skg-recovery-archive-inspect
     (plist-get skg-recovery-incident :path)))
   ((skg-recovery-ui--incident-at-point))
   (t
    (let* ((summaries (skg-recovery-archive-list))
           (selected (funcall skg-recovery-ui-incident-picker
                              summaries "Maintenance incident: ")))
      (skg-recovery-ui--resolve-incident selected)))))

(defun skg-recovery-ui--interrupted-records (summary)
  (unless (eq (plist-get summary :status) 'finalized)
    (user-error "Only finalized incidents have interrupted dispositions"))
  (let ((keys (make-hash-table :test #'equal))
        result)
    (dolist (settlement
             (skg-recovery--required-list
              (plist-get summary :final) 'buffer-dispositions
              "final manifest"))
      (when (equal (skg-recovery--required-text
                    settlement 'disposition "buffer disposition")
                   "interrupted")
        (puthash (skg-recovery--required-text
                  settlement 'buffer-key "buffer disposition")
                 t keys)))
    (dolist (record
             (skg-recovery--required-list
              (plist-get summary :initial) 'buffers "initial manifest"))
      (when (gethash (skg-recovery--required-text
                      record 'buffer-key "initial buffer")
                     keys)
        (push record result)))
    (setq result (nreverse result))
    (unless (= (length result) (hash-table-count keys))
      (signal 'skg-recovery-archive-error
              '("an interrupted disposition has no archived buffer")))
    result))

(defun skg-recovery-ui--resolve-buffer (summary &optional buffer-key)
  (let* ((records (skg-recovery-ui--interrupted-records summary))
         (current-key
          (and (derived-mode-p 'skg-recovery-mode)
               skg-recovery-buffer-record
               (skg-recovery--required-text
                skg-recovery-buffer-record 'buffer-key "initial buffer"))))
    (or (and buffer-key
             (cl-find (format "%s" buffer-key) records :test #'equal
                      :key (lambda (record)
                             (skg-recovery--required-text
                              record 'buffer-key "initial buffer"))))
        (and current-key
             (cl-find current-key records :test #'equal
                      :key (lambda (record)
                             (skg-recovery--required-text
                              record 'buffer-key "initial buffer"))))
        (funcall skg-recovery-ui-buffer-picker
                 records "Interrupted buffer: ")
        (user-error "No interrupted buffer selected"))))

(defun skg-recovery-ui--parse-exact-bytes (bytes context)
  (let* ((text (decode-coding-string bytes 'utf-8-unix)) value end)
    (unless (equal bytes (encode-coding-string text 'utf-8-unix))
      (signal 'skg-recovery-archive-error
              (list (format "%s is not exact UTF-8" context))))
    (condition-case error-data
        (pcase-let ((`(,parsed . ,position) (read-from-string text)))
          (setq value parsed end position))
      (error
       (signal 'skg-recovery-archive-error
               (list (format "invalid %s: %s" context
                             (error-message-string error-data))))))
    (unless (string-blank-p (substring text end))
      (signal 'skg-recovery-archive-error
              (list (format "%s has trailing data" context))))
    value))

(defun skg-recovery-ui--verify-buffer-artifacts (summary record)
  (let* ((root (plist-get summary :path))
         (key (skg-recovery--required-text
               record 'buffer-key "initial buffer"))
         (prefix (format "buffer-snapshots/%s/" key))
         (required '("README.org" "metadata.sexp" "last-fetched.org"
                     "unsaved-changes.org" "diff.txt"))
         (artifacts (make-hash-table :test #'equal))
         sidecar-error)
    (unless (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9._-]*\\'" key)
      (signal 'skg-recovery-archive-error
              '("unsafe archived buffer key")))
    (dolist (artifact
             (skg-recovery--required-list
              record 'artifacts "initial buffer"))
      (let ((relative (skg-recovery--required-text
                       artifact 'path "buffer artifact")))
        (unless (string-prefix-p prefix relative)
          (signal 'skg-recovery-archive-error
                  '("buffer artifact belongs outside its snapshot")))
        (if (member (file-name-nondirectory relative)
                    '("undo.emacs.gz" "undo.nvim"))
            (condition-case error-data
                (puthash relative
                         (skg-recovery--verify-recorded-artifact
                          root artifact "native undo artifact")
                         artifacts)
              (error (setq sidecar-error
                           (error-message-string error-data))))
          (puthash relative
                   (skg-recovery--verify-recorded-artifact
                    root artifact "buffer artifact")
                   artifacts))))
    (dolist (filename required)
      (unless (gethash (concat prefix filename) artifacts)
        (signal 'skg-recovery-archive-error
                (list (format "snapshot lacks verified %s" filename)))))
    (list :key key :prefix prefix :artifacts artifacts
          :sidecar-error sidecar-error)))

(defun skg-recovery-ui--artifact-link-records
    (summary buffer-record verified)
  (let ((root (plist-get summary :path)) links)
    (dolist (artifact
             (skg-recovery--required-list
              buffer-record 'artifacts "initial buffer"))
      (let ((relative (skg-recovery--required-text
                       artifact 'path "buffer artifact")))
        (when (gethash relative (plist-get verified :artifacts))
          (push (list :label relative :path (expand-file-name relative root)
                      :record artifact :context "buffer artifact")
                links))))
    (dolist (group '((root-artifacts . "incident artifact")
                     (node-artifacts . "node evidence")))
      (dolist (artifact
               (skg-recovery--required-list
                (plist-get summary :final) (car group) "final manifest"))
        (let ((relative (skg-recovery--required-text
                         artifact 'path (cdr group))))
          (push (list :label relative
                      :path (expand-file-name relative root)
                      :record artifact :context (cdr group))
                links))))
    (nreverse links)))

(defun skg-recovery-ui--metadata (record verified)
  (let* ((relative (concat (plist-get verified :prefix) "metadata.sexp"))
         (metadata (skg-recovery-ui--parse-exact-bytes
                    (gethash relative (plist-get verified :artifacts))
                    "buffer metadata")))
    (unless (and
             (= (skg-recovery--required-nonnegative-integer
                 metadata 'archive-format-version "buffer metadata")
                skg-recovery-archive-format-version)
             (equal (skg-recovery--required-text
                     metadata 'buffer-key "buffer metadata")
                    (skg-recovery--required-text
                     record 'buffer-key "initial buffer"))
             (equal (skg-recovery--required-text
                     metadata 'buffer-id "buffer metadata")
                    (skg-recovery--required-text
                     record 'buffer-id "initial buffer")))
      (signal 'skg-recovery-archive-error
              '("buffer metadata belongs to another snapshot")))
    metadata))

(defun skg-recovery-ui--restore-layout (buffer metadata)
  (with-current-buffer buffer
    (let* ((point-state (skg-recovery--required-list
                         metadata 'point "buffer metadata"))
           (saved-point (skg-recovery--required-nonnegative-integer
                         point-state 'point "buffer point")))
      (goto-char (min (max (point-min) saved-point) (point-max))))
    (dolist (fold (skg-recovery--required-list
                   metadata 'folds "buffer metadata"))
      (when (and (proper-list-p fold) (= (length fold) 3)
                 (integerp (nth 0 fold)) (integerp (nth 1 fold))
                 (< (nth 0 fold) (nth 1 fold))
                 (<= (nth 1 fold) (point-max)))
        (condition-case nil
            (org-fold-region (nth 0 fold) (nth 1 fold) t
                             (intern (format "%s" (nth 2 fold))))
          (error nil)))))
  (when-let ((window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (set-window-point window (point))
      (when-let* ((windows (skg-recovery--required-list
                            metadata 'windows "buffer metadata"))
                  (first (car windows))
                  (start (skg-recovery--required-nonnegative-integer
                          first 'window-start "buffer window")))
        (set-window-start window
                          (min (max (point-min) start) (point-max)) t)))))

(defun skg-recovery-ui--restore-native-undo
    (buffer summary record verified)
  (let* ((undo (skg-recovery--required-list record 'undo "initial buffer"))
         (status (skg-recovery--required-text undo 'status "buffer undo"))
         (client (plist-get summary :client-kind))
         (version (skg-recovery--required-text undo 'version "buffer undo"))
         (prefix (plist-get verified :prefix))
         (pseudo (expand-file-name
                  (concat prefix "unsaved-changes.org")
                  (plist-get summary :path)))
         (sidecar-relative (concat prefix "undo.emacs.gz"))
         (sidecar (expand-file-name sidecar-relative
                                    (plist-get summary :path))))
    (cond
     ((not (equal status "archived")) 'text-only-no-native-history)
     ((not (equal client "emacs"))
      (display-warning 'skg
                       "Archive came from another editor; using exact text fallback"
                       :warning)
      'text-only-other-client)
     ((plist-get verified :sidecar-error)
      (display-warning 'skg
                       (concat "Native undo is corrupt; using exact text fallback: "
                               (plist-get verified :sidecar-error))
                       :warning)
      'text-only-corrupt-sidecar)
     ((not (gethash sidecar-relative (plist-get verified :artifacts)))
      (display-warning 'skg
                       "Native undo sidecar is missing; using exact text fallback"
                       :warning)
      'text-only-missing-sidecar)
     (t
      (condition-case error-data
          (progn
            (skg-undo-sidecar-restore buffer pseudo sidecar version)
            'native-restored)
        (error
         (with-current-buffer buffer
           (setq buffer-undo-list nil pending-undo-list nil))
         (display-warning
          'skg
          (format "Native undo could not be restored; using exact text fallback: %s"
                  (error-message-string error-data))
          :warning)
         'text-only-native-error))))))

(defun skg-recovery-ui--buffer-name (summary record)
  (format "%s [archive %s/%s]"
          (skg-recovery--required-text record 'name "initial buffer")
          (substring (plist-get summary :incident-id) 0 8)
          (skg-recovery--required-text record 'buffer-key "initial buffer")))

(defun skg-open-interrupted-view (&optional incident buffer-key)
  "Open an independent checksum-verified copy of one interrupted view."
  (interactive)
  (let* ((summary (skg-recovery-ui--resolve-incident incident))
         (record (skg-recovery-ui--resolve-buffer summary buffer-key))
         (verified (skg-recovery-ui--verify-buffer-artifacts summary record))
         (metadata (skg-recovery-ui--metadata record verified))
         (relative (concat (plist-get verified :prefix)
                           "unsaved-changes.org"))
         (bytes (gethash relative (plist-get verified :artifacts)))
         (text (decode-coding-string bytes 'utf-8-unix))
         (buffer (generate-new-buffer
                  (skg-recovery-ui--buffer-name summary record))))
    (unless (equal bytes (encode-coding-string text 'utf-8-unix))
      (kill-buffer buffer)
      (signal 'skg-recovery-archive-error
              '("archived buffer text is not exact UTF-8")))
    (condition-case error-data
        (progn
          (with-current-buffer buffer
            (skg-recovery-mode)
            (buffer-disable-undo)
            (insert text)
            (buffer-enable-undo)
            (setq buffer-undo-list nil pending-undo-list nil)
            (setq skg-recovery-incident summary
                  skg-recovery-buffer-record record
                  skg-recovery-metadata metadata
                  skg-recovery-artifact-links
                  (skg-recovery-ui--artifact-link-records
                   summary record verified))
            (setq skg-recovery-native-undo-status
                  (skg-recovery-ui--restore-native-undo
                   buffer summary record verified))
            (set-buffer-modified-p nil))
          (pop-to-buffer buffer)
          (skg-recovery-ui--restore-layout buffer metadata)
          buffer)
      (error
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (signal (car error-data) (cdr error-data))))))

(defun skg-recovery-open-artifact ()
  "Open a checksum-verified artifact referenced by this recovery buffer."
  (interactive)
  (unless (and (derived-mode-p 'skg-recovery-mode)
               skg-recovery-artifact-links)
    (user-error "This recovery buffer has no artifact links"))
  (let* ((choices
          (mapcar (lambda (entry)
                    (cons (plist-get entry :label) entry))
                  skg-recovery-artifact-links))
         (choice (completing-read "Recovery artifact: " choices nil t))
         (entry (cdr (assoc choice choices))))
    (skg-recovery--verify-recorded-artifact
     (plist-get skg-recovery-incident :path)
     (plist-get entry :record) (plist-get entry :context))
    (find-file-read-only (plist-get entry :path))))

(defun skg-recovery-ui--read-recipe (record)
  (skg-recovery-ui--parse-exact-bytes
   (encode-coding-string
    (skg-recovery--required-text record 'recipe "initial buffer")
    'utf-8-unix)
   "archived view recipe"))

(defun skg-recovery-ui--recipe-value (recipe key)
  (when-let ((entry (and (listp recipe) (assq key recipe))))
    (if (and (proper-list-p entry) (= (length entry) 2))
        (cadr entry)
      (cdr entry))))

(defun skg-recovery-ui--true-p (value)
  (or (eq value t) (eq value 'true)
      (equal (format "%s" value) "true")))

(defun skg-recovery-ui--fresh-target (&optional incident buffer-key)
  (let* ((summary (skg-recovery-ui--resolve-incident incident))
         (record (skg-recovery-ui--resolve-buffer summary buffer-key)))
    (cons summary record)))

(defun skg-open-fresh-view-for-interrupted (&optional incident buffer-key)
  "Open a new live view/search from an interrupted view's archived recipe."
  (interactive)
  (pcase-let* ((`(,summary . ,record)
                (skg-recovery-ui--fresh-target incident buffer-key))
               (kind (skg-recovery--required-text
                      record 'kind "initial buffer"))
               (recipe (skg-recovery-ui--read-recipe record)))
    (cond
     ((equal kind "content-view")
      (let* ((recipe-root
              (skg-recovery-ui--recipe-value recipe 'root-id))
             (roots (delete-dups
                     (delq nil
                           (append
                            (and recipe-root
                                 (list (format "%s" recipe-root)))
                            (mapcar
                             (lambda (root) (format "%s" root))
                             (skg-recovery--required-list
                              record 'root-ids "initial buffer"))))))
             (root (cond
                    ((null roots)
                     (user-error "Archive records no content-view root"))
                    ((null (cdr roots)) (car roots))
                    (t (funcall skg-recovery-ui-root-picker
                                roots "Fresh live root: ")))))
        (skg-request-single-root-content-view-from-id root)))
     ((equal kind "search-view")
      (let ((terms (skg-recovery-ui--recipe-value recipe 'terms)))
        (unless (and terms
                     (yes-or-no-p
                      (format "Rerun archived search %S against current data? "
                              terms)))
          (user-error "Fresh search cancelled"))
        (skg--request-text-search
         (format "%s" terms)
         (skg-recovery-ui--true-p
          (skg-recovery-ui--recipe-value recipe 'regex))
         (skg-recovery-ui--true-p
          (skg-recovery-ui--recipe-value recipe 'body))
         (skg-recovery-ui--true-p
          (skg-recovery-ui--recipe-value recipe 'operators))
         (skg-recovery-ui--recipe-value recipe 'ugly-choice))))
     (t
      (user-error "Fresh recovery is unsupported for archived kind %s" kind)))
    summary))

(defun skg-recovery-ui--active-incident-id ()
  (or (and (boundp 'skg--maintenance-client-incident)
           (plist-get skg--maintenance-client-incident :incident-id))
      (and (listp skg--maintenance-state)
           (let ((value (or (cdr (assq 'incident-id skg--maintenance-state))
                            (cdr (assq 'active-incident-id
                                       skg--maintenance-state)))))
             (and value (format "%s" value))))))

(defun skg-delete-maintenance-incident (&optional incident confirmed)
  "Delete one finalized immediate-child incident after explicit confirmation.
CONFIRMED is for noninteractive callers and tests."
  (interactive)
  (let* ((summary (skg-recovery-ui--resolve-incident incident))
         (path (directory-file-name (expand-file-name
                                     (plist-get summary :path))))
         (root (skg-recovery-resolve-archive-root))
         (parent (directory-file-name (file-name-directory path))))
    (unless (eq (plist-get summary :status) 'finalized)
      (user-error "Only finalized maintenance incidents can be deleted"))
    (unless (and (equal parent root)
                 (skg-recovery--strict-final-name-p
                  (file-name-nondirectory path))
                 (not (file-symlink-p path)))
      (user-error "Incident is not a safe immediate child of the archive root"))
    (when (equal (plist-get summary :incident-id)
                 (skg-recovery-ui--active-incident-id))
      (user-error "The active maintenance incident still needs this archive"))
    (unless (or confirmed
                (yes-or-no-p
                 (format
                  (concat "Delete %s (%d bytes, %s)? "
                          "This is ordinary deletion, not secure erase. ")
                  (plist-get summary :name)
                  (plist-get summary :bytes)
                  (plist-get summary :iec))))
      (user-error "Incident deletion cancelled"))
    ;; `skg-recovery-archive-inspect' has just walked the whole tree without
    ;; accepting symlinks or special entries; PATH is the exact checked child.
    (delete-directory path t)
    (message "Deleted maintenance incident %s; ordinary deletion may remain recoverable from backups or storage"
             (plist-get summary :name))
    t))

(provide 'skg-recovery-ui)
