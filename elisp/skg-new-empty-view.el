;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-view-new-empty

(require 'skg-buffer)
(require 'skg-state)

(defun skg-owned-sources-from-toml (file)
  "Return a list of nickname strings for sources with user_owns_it = true in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((sources '())
          (current-nickname nil))
      (while (not (eobp))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))
          (cond
           ((string-match "^\\[\\[sources\\]\\]" line)
            (setq current-nickname nil))
           ((string-match
             "^nickname[ \t]*=[ \t]*\"\\([^\"]+\\)\""
             line)
            (setq current-nickname (match-string 1 line)))
           ((and current-nickname
                 (string-match
                  "^user_owns_it[ \t]*=[ \t]*true"
                  line))
            (push current-nickname sources)
            (setq current-nickname nil))))
        (forward-line 1))
      (nreverse sources))))

(defun skg-view-new-empty ()
  "Open a new skg content view for a new node.
Prompts to choose an owned source, then opens a buffer
with an indefinitive TrueNode (no ID yet) and a placeholder title."
  (interactive)
  (let* ((config-file
          (expand-file-name "skgconfig.toml" skg-config-dir))
         (owned-sources
          (skg-owned-sources-from-toml config-file))
         (source
          (if (= (length owned-sources) 1)
              (car owned-sources)
            (completing-read "Source: " owned-sources nil t)))
         (org-text
          (format
           "* (skg (node (source %s) indefinitive)) life, the universe and everything\n"
           source)))
    (skg-open-org-buffer-from-text
     nil org-text (skg-content-view-buffer-name org-text))))

(provide 'skg-view-new-empty)
