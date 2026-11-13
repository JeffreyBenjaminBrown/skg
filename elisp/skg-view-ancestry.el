;;; skg-view-ancestry.el --- Show ancestry of org headline -*- lexical-binding: t; -*-

;;; PURPOSE:
;;; This is not skg-specific; it works in any org buffer.
;;; What I mean by 'ancestry' is the node at point's parent, grandparent, etc. up to the top level.
;;; For instance, if you called this function from 'e' in the following buffer:
;;;   * a
;;;   ** b
;;;   ** c
;;;   *** d
;;;   *** e
;;;   *** f
;;; it would leave that buffer unchanged,
;;; but open a new one containing only this:
;;;   * a
;;;   ** c
;;;   *** e

(require 'org)
(require 'skg-buffer)

(defun skg-view-ancestry--buffer-name (headline-text)
  "The name of the ancestry buffer of the headline HEADLINE-TEXT:
'*Ancestry (vnid X): TITLE*' when its skg metadata carries (vnid X),
else '*Ancestry: TITLE*'. TITLE excludes the metadata."
  (let* ((parts (skg-split-as-stars-metadata-title headline-text))
         (metadata-text (nth 1 parts))
         (title (if parts (nth 2 parts) headline-text))
         (vnid (and parts (not (string-empty-p metadata-text))
                    (skg--metadata-vnid
                     (ignore-errors
                       (car (read-from-string metadata-text)))))))
    (if vnid
        (format "*Ancestry (vnid %s): %s*" vnid title)
      (format "*Ancestry: %s*" title))))

(defun skg-view-ancestry ()
  "Create a new buffer showing only the ancestry of the headline at point.
The new buffer will contain the current headline and all its ancestors,
from the root down to the current position. It shows heralds if
the buffer it was called from does."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let ((ancestry '())
        (buffer-name nil)
        (heralds (bound-and-true-p heralds-minor-mode)))
    (save-excursion
      (condition-case nil
          (unless (org-at-heading-p)
            (org-back-to-heading t))
        (error (user-error "Not on a headline")))
      (setq buffer-name (skg-view-ancestry--buffer-name
                         (skg-get-current-headline-text)))
      ;; Collect current headline and all ancestors
      (while (progn
               (push (cons (org-current-level)
                           (org-get-heading t t t t))
                     ancestry)
               (condition-case nil
                   (progn (outline-up-heading 1 t) t)
                 (error nil)))))
    ;; Create new buffer with ancestry
    (let ((buf (generate-new-buffer buffer-name)))
      (with-current-buffer buf
        (skg--org-mode-with-options)
        (dolist (item ancestry)
          (insert (make-string (car item) ?*)
                  " "
                  (cdr item)
                  "\n"))
        (when heralds (heralds-minor-mode 1))
        (goto-char (point-min)))
      (switch-to-buffer buf))))

(provide 'skg-view-ancestry)
