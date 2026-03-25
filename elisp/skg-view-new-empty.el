;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-view-new-empty

(require 'skg-buffer)
(require 'skg-config)

(defun skg-view-new-empty ()
  "Open a new skg content view for a new node.
Prompts to choose an owned source, then opens a buffer
with an indefinitive TrueNode (no ID yet) and a placeholder title."
  (interactive)
  (let* ((owned-sources (skg--owned-sources))
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
