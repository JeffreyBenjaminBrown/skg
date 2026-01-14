;;; org-ancestry.el --- Show ancestry of org headline -*- lexical-binding: t; -*-

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

(defun org-ancestry ()
  "Create a new buffer showing only the ancestry of the headline at point.
The new buffer will contain the current headline and all its ancestors,
from the root down to the current position."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let ((ancestry '()))
    (save-excursion
      (condition-case nil
          (unless (org-at-heading-p)
            (org-back-to-heading t))
        (error (user-error "Not on a headline")))
      ;; Collect current headline and all ancestors
      (while (progn
               (push (cons (org-current-level)
                           (org-get-heading t t t t))
                     ancestry)
               (condition-case nil
                   (progn (outline-up-heading 1 t) t)
                 (error nil)))))
    ;; Create new buffer with ancestry
    (let* ((current-heading (cdr (car (last ancestry))))
           (buf (generate-new-buffer
                 (format "*Ancestry: %s*" current-heading))))
      (with-current-buffer buf
        (org-mode)
        (dolist (item ancestry)
          (insert (make-string (car item) ?*)
                  " "
                  (cdr item)
                  "\n"))
        (goto-char (point-min)))
      (switch-to-buffer buf))))

(provide 'org-ancestry)
