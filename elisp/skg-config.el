;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read configuration from skgconfig.toml.

(require
 ;; PITFALL:
 ;; A tiny bit of config-related code is found outside this module:
 ;; The variable `skg-config-dir' (the directory containing skgconfig.toml)
 ;; is defined in skg-state.el, alongside the other global state.
 ;; It is set once, by `skg-client-init'.
 'skg-state)

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

(defun skg--owned-sources ()
  "Return the list of owned source names from skgconfig.toml, or nil."
  (when skg-config-dir
    (let ((config-file
           (expand-file-name "skgconfig.toml" skg-config-dir)))
      (when (file-exists-p config-file)
        (skg-owned-sources-from-toml config-file)))))

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
  (when skg-config-dir
    (let ((config-file
           (expand-file-name "skgconfig.toml" skg-config-dir)))
      (when (file-exists-p config-file)
        (cdr (assoc source-name
                    (skg-source-paths-from-toml config-file)))))))

(defun skg--abs-path-for-id-and-source (id source)
  "Return the absolute path of ID.skg within SOURCE's directory,
or nil if SOURCE is not declared in the config."
  (let ((dir (skg--source-dir source)))
    (when dir
      (expand-file-name (concat id ".skg") dir))))

(provide 'skg-config)
