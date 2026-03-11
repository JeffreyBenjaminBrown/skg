;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: User-facing command aliases.
;;; Some internal function names reflect implementation details,
;;; yet are user-facing. This file provides short,
;;; memorable user-facing names for those functions.
;;; (Not every user-facing command is involved in this file --
;;; only the ones that need aliases.)

;; No requires here — the underlying functions are loaded by skg-client.el,
;; which also requires this file. Adding requires here would create a cycle.

(setq ;; By default M-x shows every interactive command, ignoring `completion-predicate'. This passage makes Emacs actually consult each command's `completion-predicate' property — thus letting `skg-alias' hides internal names. This setting is global (not skg-specific), and it enables Emacs's own mode-aware filtering for built-in commands.
 read-extended-command-predicate
 #'command-completion-default-include-p)

(defmacro skg-alias (alias internal)
  "Define ALIAS as a user-facing name for INTERNAL,
and hide INTERNAL from M-x completion."
  `(progn
     (defalias ',alias ',internal)
     (put ',internal 'completion-predicate #'ignore)) )

(skg-alias skg-view-from-node-id skg-request-single-root-content-view-from-id)
(skg-alias skg-save              skg-request-save-buffer)
(skg-alias skg-metadata-view     skg-edit-first-sexpr-on-line)
(skg-alias skg-heralds-view      heralds-minor-mode)
(skg-alias skg-branch-aliases       skg-request-aliases-view)
(skg-alias skg-branch-containerward skg-request-containerward-view)
(skg-alias skg-branch-sourceward    skg-request-sourceward-view)
(skg-alias skg-definitive-view      skg-request-definitive-view)

(provide 'skg-user-facing-aliases)
