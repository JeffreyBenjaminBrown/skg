;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read configuration from skgconfig.toml.

(require 'cl-lib)
(require 'org)
(require 'skg-buffer-registry)

(require
 ;; PITFALL:
 ;; A tiny bit of config-related code is found outside this module:
 ;; The variable `skg-config-dir' (the directory containing skgconfig.toml)
 ;; is defined in skg-state.el, alongside the other global state.
 ;; It is set once, by `skg-client-init'.
 'skg-state)

(defvar skg--server-source-inventory nil
  "Normalized source entries received from the connected server.
Nil before connection; local TOML readers are the startup fallback.")

(defvar skg--server-store-state nil
  "Authoritative graph generation, path outcomes and store health from server.")

(defun skg-observe-server-graph-generation (generation)
  "Advance the connection's selected graph baseline to GENERATION.
GENERATION observations can arrive on ordinary operation responses as well as
the connection handshake.  Never regress the baseline if an older response is
handled after a newer one."
  (when (natnump generation)
    (let ((current (cdr (assq 'graph-generation skg--server-store-state))))
      (when (or (not (natnump current)) (> generation current))
        (setf (alist-get 'graph-generation skg--server-store-state)
              generation)))))

(defun skg--atom-string (value)
  "Normalize a wire atom VALUE to a string, preserving nil."
  (when value (format "%s" value)))

(defun skg-install-source-inventory (response)
  "Install the normalized source inventory carried by RESPONSE."
  (let ((wire-entries (cadr (assoc 'source-inventory response))))
    (when wire-entries
      (setq skg--server-source-inventory
            (mapcar
             (lambda (entry)
               (list :name (skg--atom-string
                            (cadr (assoc 'name entry)))
                     :abbreviation (skg--atom-string
                                    (cadr (assoc 'abbreviation entry)))
                     :owned (not (null (cadr (assoc 'owned entry))))
                     :position (cadr (assoc 'position entry))
                     :configured-path
                     (skg--atom-string
                      (cadr (assoc 'configured-path entry)))
                     :directory
                     (skg--atom-string (cadr (assoc 'directory entry)))
                     :directory-identity
                     (skg--atom-string
                      (cadr (assoc 'directory-identity entry)))))
             wire-entries))
      (when (fboundp 'skg-register-open-raw-file-buffers)
        (skg-register-open-raw-file-buffers)))))

(defun skg-config-file ()
  "Return the skgconfig.toml path for `skg-config-dir', or nil."
  (when skg-config-dir
    (let ((config-file
           (expand-file-name "skgconfig.toml" skg-config-dir)))
      (when (file-exists-p config-file)
        config-file))))

(defun skg--completing-read-with-cycle
    (prompt collection &optional predicate require-match
            initial-input hist def inherit-input-method cycle-values
            extra-minibuffer-setup)
  "Call `completing-read' with S-left/S-right cycling through CYCLE-VALUES.
The usual `completing-read' arguments keep their ordinary meaning.
EXTRA-MINIBUFFER-SETUP, if non-nil, runs inside the minibuffer setup
hook after the cycle bindings are installed."
  (let ((cycle (lambda (dir)
                 (lambda ()
                   (interactive)
                   (when cycle-values
                     (let* ((cur (minibuffer-contents))
                            (idx (or (cl-position cur cycle-values
                                                  :test #'string=)
                                     (and initial-input
                                          (cl-position initial-input
                                                       cycle-values
                                                       :test #'string=))
                                     0))
                            (new (nth (mod (+ idx dir)
                                           (length cycle-values))
                                      cycle-values)))
                       (delete-minibuffer-contents)
                       (insert new)))))))
    (minibuffer-with-setup-hook
        (lambda ()
          (when cycle-values
            (local-set-key (kbd "S-<left>")  (funcall cycle -1))
            (local-set-key (kbd "S-<right>") (funcall cycle  1)))
          (when extra-minibuffer-setup
            (funcall extra-minibuffer-setup)))
      (completing-read
       prompt collection predicate require-match initial-input hist def
       inherit-input-method))))

(defun skg--prompt-for-owned-source ()
  "Prompt the user to choose an owned source, with S-left/S-right cycling.
If there is only one owned source, return it without prompting."
  (let ((owned-sources (skg--owned-sources)))
    (if (= (length owned-sources) 1)
        (car owned-sources)
      (skg--completing-read-with-cycle
       "Source: " owned-sources nil t nil nil nil nil owned-sources))))

(defun skg--prompt-for-source-change (current-source)
  "Prompt for a source to replace CURRENT-SOURCE.
S-left/S-right cycle through owned sources, and C-? displays every
configured source with its path.  The user may also type a source
name directly."
  (let ((owned-sources (skg--owned-sources))
        (source-names (skg--source-names)))
    (skg--completing-read-with-cycle
     "Source (S-left/right cycle; press C-? for a list of sources): "
     source-names nil nil current-source nil nil nil owned-sources
     (lambda ()
       (local-set-key (kbd "C-?") #'skg-view-source-list)))))

(defun skg--prompt-for-source-set ()
  "Prompt for a source-set choice, with completion and S-left/S-right cycling.
A source-set is a prefix of the config's privacy order: choosing a
source makes it and everything more public available; \"all\" makes
everything available."
  (let ((source-sets (skg--source-set-names)))
    (unless source-sets
      (user-error "No skg sources found"))
    (skg--completing-read-with-cycle
     "Most private source to make available (S-left/right cycle): "
     source-sets nil t nil nil "all" nil source-sets)))

(defun skg--source-paths ()
  "Return (source-name . absolute-path) entries for this server."
  (if skg--server-source-inventory
      (mapcar (lambda (entry)
                (cons (plist-get entry :name)
                      (plist-get entry :directory)))
              skg--server-source-inventory)
    (let ((config-file (skg-config-file)))
      (when config-file
        (skg-source-paths-from-toml config-file)))))

(defun skg--source-names ()
  "Return configured source names in privacy order."
  (if skg--server-source-inventory
      (mapcar (lambda (entry) (plist-get entry :name))
              skg--server-source-inventory)
    (let ((config-file (skg-config-file)))
      (when config-file
        (skg-source-names-from-toml config-file)))))

(defun skg--source-set-names ()
  "Return the source-set choices, in privacy order, ending with \"all\".
The choices are the configured source names (each meaning that
source and everything more public -- a prefix of the config's
privacy order) plus the reserved \"all\"."
  (let ((names (skg--source-names)))
    (when names (append names (list "all")))))

(defun skg-view-source-list ()
  "Display an org buffer listing configured sources and their paths."
  (interactive)
  (let ((source-paths (skg--source-paths)))
    (unless source-paths
      (user-error "No skg sources found"))
    (let ((buffer (skg-acquire-generated-buffer "*skg-sources*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (source-path source-paths)
            (insert (format "* %s\n%s\n"
                            (car source-path)
                            (cdr source-path)))))
        (org-mode)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (skg-register-buffer
         buffer 'derived-report :lifecycle 'client-local :disposable t
         :recipe '((kind . "source-list"))
         :last-fetched (skg-buffer-raw-text buffer)))
      (display-buffer buffer))))

(defun skg--owned-sources ()
  "Return owned source names in privacy order, or nil."
  (if skg--server-source-inventory
      (delq nil
            (mapcar (lambda (entry)
                      (when (plist-get entry :owned)
                        (plist-get entry :name)))
                    skg--server-source-inventory))
    (let ((config-file (skg-config-file)))
      (when config-file
        (skg-owned-sources-from-toml config-file)))))

(defun skg-port-from-toml (file)
  "Return the integer value of `port = ...` from FILE (a TOML config)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward
         "^[ \t]*port[ \t]*=[ \t]*\\([0-9]+\\)"
         nil t)
        (string-to-number (match-string 1))
      (error "No port setting found in %s" file)) ))

(defun skg-owned-folder-from-toml (file)
  "Return the `owned_folder' setting from FILE, defaulting to \"owned\".
Sources whose path sits under this folder are owned; all others are
foreign. Replaces the retired per-source `user_owns_it' key."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward
         "^[ \t]*owned_folder[ \t]*=[ \t]*\"\\([^\"]+\\)\""
         nil t)
        (match-string 1)
      "owned")))

(defun skg-owned-sources-from-toml (file)
  "Return the names of the OWNED sources in FILE, in declaration order.
A source is owned iff its path (resolved against FILE's directory,
the data root) sits under the data root's `owned_folder' (default
\"owned\") -- the author-folder layout, mirroring the server's
`derive_ownership_and_labels'. A source with no `name' key defaults
its name to its path, also mirroring the server."
  (let* ((owned-folder (skg-owned-folder-from-toml file))
         (data-root (file-name-directory file))
         (owned-root (file-name-as-directory
                      (expand-file-name owned-folder data-root))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((sources '())
            (in-sources nil)
            (current-name nil)
            (current-path nil)
            (flush nil))
        (setq flush
              (lambda ()
                (when (and in-sources current-path)
                  (let ((abs (file-name-as-directory
                              (expand-file-name current-path data-root))))
                    (when (or (equal abs owned-root)
                              (string-prefix-p owned-root abs))
                      (push (or current-name current-path) sources))))
                (setq current-name nil current-path nil)))
        (while (not (eobp))
          (let ((line (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
            (cond
             ((string-match "^\\[\\[sources\\]\\]" line)
              (funcall flush)
              (setq in-sources t))
             ((string-match "^\\[\\[" line) ;; a different array table
              (funcall flush)
              (setq in-sources nil))
             ((and in-sources
                   (string-match
                    "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
              (setq current-name (match-string 1 line)))
             ((and in-sources
                   (string-match
                    "^path[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
              (setq current-path (match-string 1 line)))))
          (forward-line 1))
        (funcall flush)
        (nreverse sources)))))

(defun skg--toml-array-table-line-name (line)
  "Return the array-table name from LINE, or nil if LINE is not one."
  (when (string-match "^\\[\\[\\([[:alnum:]_-]+\\)\\]\\]" line)
    (match-string 1 line)))

(defun skg-table-names-from-toml (file table-name)
  "Return name strings from each [[TABLE-NAME]] entry in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((names '())
          (current-table nil))
      (while (not (eobp))
        (let* ((line (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
               (new-table (skg--toml-array-table-line-name line)))
          (cond
           (new-table
            (setq current-table new-table))
           ((and (equal current-table table-name)
                 (string-match "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
            (push (match-string 1 line) names))))
        (forward-line 1))
      (nreverse names))))

(defun skg-source-names-from-toml (file)
  "Return effective configured source names from FILE.
An unnamed source's name is its path string, matching the server."
  (mapcar #'car (skg-source-paths-from-toml file)))

(defun skg-source-paths-from-toml (file)
  "Return an alist of (name . absolute-dir) for each [[sources]] entry
in FILE. Relative source paths are resolved against the directory of
FILE, matching what the server's `make_paths_absolute' does at
config-load time."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let* ((dir (file-name-directory file))
           (result    '())
           (current-table nil)
           (cur-name      nil)
           (cur-path      nil)
           (flush (lambda ()
                    (when cur-path
                      (push (cons (or cur-name cur-path)
                                  (expand-file-name cur-path dir))
                            result)
                      (setq cur-name nil cur-path nil)))))
      (while (not (eobp))
        (let* ((line (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
               (new-table (skg--toml-array-table-line-name line)))
          (cond
           (new-table
            (funcall flush)
            (setq current-table new-table)
            (setq cur-name nil cur-path nil))
           ((and (equal current-table "sources")
                 (string-match "^path[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
            (setq cur-path (match-string 1 line)))
           ((and (equal current-table "sources")
                 (string-match "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
            (setq cur-name (match-string 1 line)))))
        (forward-line 1))
      (funcall flush)
      (nreverse result))))

(defun skg--source-dir (source-name)
  "Return absolute directory for SOURCE-NAME per skgconfig.toml, or nil."
  (cdr (assoc source-name (skg--source-paths))))

(defun skg--abs-path-for-id-and-source (id source)
  "Return the absolute path of ID.skg within SOURCE's directory,
or nil if SOURCE is not declared in the config."
  (let ((dir (skg--source-dir source)))
    (when dir
      (expand-file-name (concat id ".skg") dir))))

(provide 'skg-config)
