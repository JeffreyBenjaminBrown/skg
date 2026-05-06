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

(defun skg--prompt-for-owned-source ()
  "Prompt the user to choose an owned source, with S-left/S-right cycling.
If there is only one owned source, return it without prompting."
  (let ((owned-sources (skg--owned-sources)))
    (if (= (length owned-sources) 1)
        (car owned-sources)
      (let ((cycle (lambda (dir)
                     (lambda ()
                       (interactive)
                       (let* ((cur (minibuffer-contents))
                              (idx (or (cl-position cur owned-sources
                                                    :test #'string=) 0))
                              (new (nth (mod (+ idx dir)
                                             (length owned-sources))
                                        owned-sources)))
                         (delete-minibuffer-contents)
                         (insert new))))))
        (minibuffer-with-setup-hook
            (lambda ()
              (local-set-key (kbd "S-<left>")  (funcall cycle -1))
              (local-set-key (kbd "S-<right>") (funcall cycle  1)))
          (completing-read "Source: " owned-sources nil t))))))

(defun skg--prompt-for-source-change (current-source)
  "Prompt for a source to replace CURRENT-SOURCE.
S-left/S-right cycle through owned sources, and C-? displays every
configured source with its path.  The user may also type a source
name directly."
  (let ((owned-sources (skg--owned-sources))
        (source-names (skg--source-names)))
    (let ((cycle (lambda (dir)
                   (lambda ()
                     (interactive)
                     (let* ((cur (minibuffer-contents))
                            (idx (or (cl-position cur owned-sources
                                                  :test #'string=)
                                     (cl-position current-source owned-sources
                                                  :test #'string=)
                                     0))
                            (new (nth (mod (+ idx dir)
                                           (length owned-sources))
                                      owned-sources)))
                       (delete-minibuffer-contents)
                       (insert new))))))
      (minibuffer-with-setup-hook
          (lambda ()
            (when owned-sources
              (local-set-key (kbd "S-<left>")  (funcall cycle -1))
              (local-set-key (kbd "S-<right>") (funcall cycle  1)))
            (local-set-key (kbd "C-?") #'skg-view-source-list))
        (completing-read
         "Source (S-left/right cycle; press C-? for a list of sources): "
         source-names nil nil current-source)))))

(defun skg--source-paths ()
  "Return an alist of (source-name . absolute-path) from skgconfig.toml."
  (let ((config-file (skg-config-file)))
    (when config-file
      (skg-source-paths-from-toml config-file))))

(defun skg--source-names ()
  "Return the configured source names from skgconfig.toml."
  (mapcar #'car (skg--source-paths)))

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

(defun skg--default-source ()
  "Return the first owned source name from config, or nil."
  (car (skg--owned-sources)))

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
           (cur-name  nil)
           (cur-path  nil)
           (flush (lambda ()
                    (when (and cur-name cur-path)
                      (push (cons cur-name
                                  (expand-file-name cur-path dir))
                            result)
                      (setq cur-name nil cur-path nil)))))
      (while (not (eobp))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))
          (cond
           ((string-match "^\\[\\[sources\\]\\]" line)
            (funcall flush))
           ((string-match "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line)
            (setq cur-name (match-string 1 line)))
           ((string-match "^path[ \t]*=[ \t]*\"\\([^\"]+\\)\"" line)
            (setq cur-path (match-string 1 line)))))
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
