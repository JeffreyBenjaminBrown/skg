;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read configuration from skgconfig.toml.

(require 'cl-lib)
(require 'org)

(require
 ;; PITFALL:
 ;; A tiny bit of config-related code is found outside this module:
 ;; The variables `skg-config-file-path' (the config file's path)
 ;; and `skg-config-dir' (the directory containing it)
 ;; are defined in skg-state.el, alongside the other global state.
 ;; They are set once, by `skg-client-init'.
 'skg-state)

(defun skg-config-file ()
  "Return `skg-config-file-path' if it names an existing file, or nil."
  (when (and skg-config-file-path
             (file-exists-p skg-config-file-path))
    skg-config-file-path))

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

(defconst skg--foreign-repos-choices
  '(("foreign: shown" . foreign-shown)
    ("foreign: hidden" . foreign-hidden))
  "The skgrepo restriction picker's foreign-repos choices: label . choice.")

(defun skg--prompt-for-repo-set (&optional offer-foreign-repos)
  "Prompt for the skgrepo restriction, with completion and S-left/S-right cycling.
The skgrepo restriction's rung is the most private owned repo to
show: choosing one shows it and every more public owned repo.  Each
choice is shown by its name, the most private one with \" (no
restriction)\" appended, the default.  Returns the chosen repo's name.
When OFFER-FOREIGN-REPOS is non-nil and the config has a foreign
repo, the choices also include \"foreign: shown\" and \"foreign:
hidden\", which return the symbol `foreign-shown' or
`foreign-hidden': foreign repos are shown or hidden apart from the
rung."
  (let* ((names (skg--repo-set-names))
         (rung-labels (mapcar #'skg--skgrepo-restriction-label names))
         (foreign-choices (when (and offer-foreign-repos
                                     (skg--foreign-repo-names))
                            skg--foreign-repos-choices))
         (labels (append rung-labels (mapcar #'car foreign-choices))))
    (unless names
      (user-error "No owned skg repos found"))
    (let ((label (skg--completing-read-with-cycle
                  (if foreign-choices
                      "Most private repo to show, or foreign: shown/hidden (S-left/right cycle): "
                    "Most private repo to show (S-left/right cycle): ")
                  labels nil t nil nil (car (last rung-labels)) nil labels)))
      (or (cdr (assoc label foreign-choices))
          (nth (cl-position label rung-labels :test #'string=) names)))))

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
  "Return the skgrepo restriction's rungs: the owned repo names, in privacy order.
Each names the most private owned repo to show, that repo and every
more public owned one (a prefix of the config's privacy ladder); the
last, with foreign repos shown, is no skgrepo restriction.  Foreign
repos are on no rung, wherever the config lists them."
  (skg--owned-repos))

(defun skg--foreign-repo-names ()
  "Return the names of the configured foreign repos, in config order, or nil."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-foreign-repos-from-toml config-file))))

(defun skg--skgrepo-restriction-label (name)
  "How the clients show the skgrepo restriction's rung NAME, a repo name.
The config's most private owned repo, which with foreign repos shown
shows every repo, gets \" (no restriction)\" appended; the words never
reach the server."
  (if (equal name (car (last (skg--owned-repos))))
      (concat name " (no restriction)")
    name))

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

(defun skg-owned-repos-from-toml (file)
  "Return the names of the owned repos in FILE, in declaration order.
That order is the privacy ladder, wherever foreign entries sit among
them.  A repo is owned unless its entry says `foreign = true',
mirroring the server. A repo with no `name' key defaults its name to
its path, also mirroring the server."
  (skg--repo-names-by-ownership-from-toml file t))

(defun skg-foreign-repos-from-toml (file)
  "Return the names of the foreign repos in FILE, in declaration order.
Named as `skg-owned-repos-from-toml' names them."
  (skg--repo-names-by-ownership-from-toml file nil))

(defun skg--repo-names-by-ownership-from-toml (file owned)
  "Return the names of the owned repos in FILE if OWNED, else the foreign ones.
In declaration order; see `skg-owned-repos-from-toml'."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((skgrepos '())
          (in-skgrepos nil)
          (current-name nil)
          (current-path nil)
          (current-foreign nil)
          (flush nil))
      (setq flush
            (lambda ()
              (when (and in-skgrepos current-path
                         (if owned (not current-foreign) current-foreign))
                (push (or current-name current-path) skgrepos))
              (setq current-name nil current-path nil current-foreign nil)))
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
            (setq current-path (match-string 1 line)))
           ((and in-skgrepos
                 (string-match "^foreign[ \t]*=[ \t]*true\\b" line))
            (setq current-foreign t))))
        (forward-line 1))
      (funcall flush)
      (nreverse skgrepos))))

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

(provide 'skg-config)
