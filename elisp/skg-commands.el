;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: User-facing command aliases.
;;; The internal function names reflect implementation details
;;; (e.g. skg-request-title-matches). This file provides
;;; short, memorable names for interactive use.

;; No requires here — the underlying functions are loaded by skg-client.el,
;; which also requires this file. Adding requires here would create a cycle.

(defmacro skg-alias (alias internal)
  "Define ALIAS as a user-facing name for INTERNAL,
and hide INTERNAL from M-x completion."
  `(progn
     (defalias ',alias ',internal)
     (put ',internal 'completion-predicate #'ignore)) )

(skg-alias skg-search-titles     skg-request-title-matches)
(skg-alias skg-view-from-node-id skg-request-single-root-content-view-from-id)
(skg-alias skg-save              skg-request-save-buffer)
(skg-alias skg-metadata-view     skg-edit-first-sexpr-on-line)
(skg-alias skg-heralds-view      heralds-minor-mode)
(skg-alias skg-branch-aliases       skg-request-aliases-view)
(skg-alias skg-branch-containerward skg-request-containerward-view)
(skg-alias skg-branch-sourceward    skg-request-sourceward-view)
(skg-alias skg-definitive-view      skg-request-definitive-view)

(provide 'skg-commands)
