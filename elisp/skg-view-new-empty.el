;;; -*- lexical-binding: t; -*-

(require 'skg-buffer)
(require 'skg-config)
(require 'skg-request-verify-connection)

(defun skg-view-new-empty ()
  "Open a new skg content view for a new node.
Prompts to choose an owned source, then opens a buffer
with an indefinitive ActiveNode (no ID yet) and a placeholder title."
  (interactive)
  (skg-tcp-connect-to-rust)
  (unless (skg-connection-handshake-ensure)
    (user-error "Cannot create a view before server verification completes"))
  (unless (and (eq skg--graph-write-admission 'open)
               (eq skg--client-constructor-admission 'open))
    (user-error "Cannot create an editable view while graph admission is closed"))
  (let* ((source (skg--prompt-for-owned-source))
         (org-text
          (format
           "* (skg (node (source %s) indef)) life, the universe and everything\n"
           source)))
    (skg-open-org-buffer-from-text
     nil org-text (skg-content-view-buffer-name org-text)
     nil 'new-empty-content-view '((kind . "new-empty"))
     '(:view-write-authority editable))))

(provide 'skg-view-new-empty)
