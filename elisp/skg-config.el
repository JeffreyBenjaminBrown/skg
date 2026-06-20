;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read configuration from skgconfig.toml.

(require 'cl-lib)
(require 'org)

(require
 ;; PITFALL:
 ;; A tiny bit of config-related code is found outside this module:
 ;; The variable `skg-config-dir' (the directory containing skgconfig.toml)
 ;; is defined in skg-state.el, alongside the other global state.
 ;; It is set once, by `skg-client-init'.
 'skg-state)

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
  "Prompt for a source-set name, with completion and S-left/S-right cycling."
  (let ((source-sets (skg--source-set-names)))
    (unless source-sets
      (user-error "No skg source-sets found"))
    (skg--completing-read-with-cycle
     "Source-set (S-left/right cycle): "
     source-sets nil t nil nil "all" nil source-sets)))

(defun skg--source-paths ()
  "Return an alist of (source-name . absolute-path) from skgconfig.toml."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-source-paths-from-toml config-file))))

(defun skg--source-names ()
  "Return the configured source names from skgconfig.toml."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-source-names-from-toml config-file))))

(defun skg--source-set-names ()
  "Return configured source-set names from skgconfig.toml, including all."
  (let ((config-file (skg-config-file)))
    (when config-file
      (cons "all" (skg-source-set-names-from-toml config-file)))))

(defun skg-view-source-list ()
  "Display an org buffer listing configured sources and their paths."
  (interactive)
  (let ((source-paths (skg--source-paths)))
    (unless source-paths
      (user-error "No skg sources found"))
    (let ((buffer (get-buffer-create "*skg-sources*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (source-path source-paths)
            (insert (format "* %s\n%s\n"
                            (car source-path)
                            (cdr source-path)))))
        (org-mode)
        (goto-char (point-min)))
      (display-buffer buffer))))

(defun skg--owned-sources ()
  "Return the list of owned source names from skgconfig.toml, or nil."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-owned-sources-from-toml config-file))))

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

(defun skg-owned-sources-from-toml (file)
  "Return a list of name strings for sources with user_owns_it = true in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((sources '())
          (current-name nil))
      (while (not (eobp))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))
          (cond
           ((string-match "^\\[\\[sources\\]\\]" line)
            (setq current-name nil))
           ((string-match
             "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\""
             line)
            (setq current-name (match-string 1 line)))
           ((and current-name
                 (string-match
                  "^user_owns_it[ \t]*=[ \t]*true"
                  line))
            (push current-name sources)
            (setq current-name nil))))
        (forward-line 1))
      (nreverse sources))))

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
  "Return configured source names from FILE."
  (skg-table-names-from-toml file "sources"))

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
                    (when (and cur-name cur-path)
                      (push (cons cur-name
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

(defun skg-source-set-names-from-toml (file)
  "Return configured source-set names from FILE.
The reserved source-set `all' is intentionally not included here;
callers that offer source-set choices should add it explicitly."
  (skg-table-names-from-toml file "source_sets"))

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
