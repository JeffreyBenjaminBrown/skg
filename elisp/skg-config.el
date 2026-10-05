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

(defun skg--prompt-for-owned-repo (&optional prompt)
  "Prompt the user to choose an owned repo, with S-left/S-right cycling.
PROMPT defaults to \"Repo: \".
If there is only one owned repo, return it without prompting."
  (let ((owned-skgrepos (skg--owned-repos)))
    (if (= (length owned-skgrepos) 1)
        (car owned-skgrepos)
      (skg--completing-read-with-cycle
       (or prompt "Repo: ") owned-skgrepos nil t nil nil nil nil owned-skgrepos))))

(defun skg--prompt-for-repo-change (current-skgrepo)
  "Prompt for a repo to replace CURRENT-REPO.
S-left/S-right cycle through owned repos, and C-? displays every
configured repo with its path.  The user may also type a repo
name directly."
  (let ((owned-skgrepos (skg--owned-repos))
        (repo-names (skg--repo-names)))
    (skg--completing-read-with-cycle
     "Repo (S-left/right cycle; press C-? for a list of repos): "
     repo-names nil nil current-skgrepo nil nil nil owned-skgrepos
     (lambda ()
       (local-set-key (kbd "C-?") #'skg-view-repo-list)))))

(defun skg--prompt-for-repo-set ()
  "Prompt for a repo-set choice, with completion and S-left/S-right cycling.
A repo-set is a prefix of the config's privacy order: choosing a
repo makes it and everything more public available; \"all\" makes
everything available."
  (let ((repo-sets (skg--repo-set-names)))
    (unless repo-sets
      (user-error "No skg repos found"))
    (skg--completing-read-with-cycle
     "Most private repo to make available (S-left/right cycle): "
     repo-sets nil t nil nil "all" nil repo-sets)))

(defun skg--repo-paths ()
  "Return an alist of (repo-name . absolute-path) from skgconfig.toml."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-repo-paths-from-toml config-file))))

(defun skg--repo-names ()
  "Return the configured repo names from skgconfig.toml."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-repo-names-from-toml config-file))))

(defun skg--repo-set-names ()
  "Return the repo-set choices, in privacy order, ending with \"all\".
The choices are the configured repo names (each meaning that
repo and everything more public -- a prefix of the config's
privacy order) plus the reserved \"all\"."
  (let ((config-file (skg-config-file)))
    (when config-file
      (append (skg-repo-names-from-toml config-file)
              (list "all")))))

(defun skg-view-repo-list ()
  "Display an org buffer listing configured repos and their paths."
  (interactive)
  (let ((repo-paths (skg--repo-paths)))
    (unless repo-paths
      (user-error "No skg repos found"))
    (let ((buffer (get-buffer-create "*skg-repos*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (repo-path repo-paths)
            (insert (format "* %s\n%s\n"
                            (car repo-path)
                            (cdr repo-path)))))
        (org-mode)
        (goto-char (point-min)))
      (display-buffer buffer))))

(defun skg--owned-repos ()
  "Return the list of owned repo names from skgconfig.toml, or nil."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-owned-repos-from-toml config-file))))

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
Repos whose path sits under this folder are owned; all others are
foreign. Replaces the retired per-repo `user_owns_it' key."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward
         "^[ \t]*owned_folder[ \t]*=[ \t]*\"\\([^\"]+\\)\""
         nil t)
        (match-string 1)
      "owned")))

(defun skg-owned-repos-from-toml (file)
  "Return the names of the OWNED repos in FILE, in declaration order.
A repo is owned iff its path (resolved against FILE's directory,
the data root) sits under the data root's `owned_folder' (default
\"owned\") -- the author-folder layout, mirroring the server's
`derive_ownership_and_labels'. A repo with no `name' key defaults
its name to its path, also mirroring the server."
  (let* ((owned-folder (skg-owned-folder-from-toml file))
         (data-root (file-name-directory file))
         (owned-root (file-name-as-directory
                      (expand-file-name owned-folder data-root))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((skgrepos '())
            (in-skgrepos nil)
            (current-name nil)
            (current-path nil)
            (flush nil))
        (setq flush
              (lambda ()
                (when (and in-skgrepos current-path)
                  (let ((abs (file-name-as-directory
                              (expand-file-name current-path data-root))))
                    (when (or (equal abs owned-root)
                              (string-prefix-p owned-root abs))
                      (push (or current-name current-path) skgrepos))))
                (setq current-name nil current-path nil)))
        (while (not (eobp))
          (let ((line (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
            (cond
             ((string-match "^\\[\\[repos\\]\\]" line)
              (funcall flush)
              (setq in-skgrepos t))
             ((string-match "^\\[\\[" line) ;; a different array table
              (funcall flush)
              (setq in-skgrepos nil))
             ((and in-skgrepos
                   (string-match
                    "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
              (setq current-name (match-string 1 line)))
             ((and in-skgrepos
                   (string-match
                    "^path[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
              (setq current-path (match-string 1 line)))))
          (forward-line 1))
        (funcall flush)
        (nreverse skgrepos)))))

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

(defun skg-repo-names-from-toml (file)
  "Return configured repo names from FILE."
  (skg-table-names-from-toml file "repos"))

(defun skg-repo-paths-from-toml (file)
  "Return an alist of (name . absolute-dir) for each [[repos]] entry
in FILE. Relative repo paths are resolved against the directory of
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
           ((and (equal current-table "repos")
                 (string-match "^path[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
            (setq cur-path (match-string 1 line)))
           ((and (equal current-table "repos")
                 (string-match "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line))
            (setq cur-name (match-string 1 line)))))
        (forward-line 1))
      (funcall flush)
      (nreverse result))))

(defun skg--repo-dir (repo-name)
  "Return absolute directory for REPO-NAME per skgconfig.toml, or nil."
  (cdr (assoc repo-name (skg--repo-paths))))

(defun skg--abs-path-for-id-and-repo (id skgrepo)
  "Return the absolute path of ID.skg within REPO's directory,
or nil if REPO is not declared in the config."
  (let ((dir (skg--repo-dir skgrepo)))
    (when dir
      (expand-file-name (concat id ".skg") dir))))

(provide 'skg-config)
