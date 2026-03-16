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

(defvar skg-content-view-mode-map
  (let (( map (make-sparse-keymap) ))
    (progn;; save
      (define-key map (kbd "C-x C-s") #'skg-request-save-buffer))
    (progn;; title text search
      (define-key map (kbd "C-c t t") #'skg-search-titles)
      (define-key map (kbd "C-c t e") #'skg-search-titles-everywhere))
    (progn;; view
      (define-key map (kbd "C-c v v") #'skg-view)
      (define-key map (kbd "C-c v i") #'skg-view-id)
      (define-key map (kbd "C-c v n") #'skg-view-new-empty)
      (define-key map (kbd "C-c v h") #'skg-view-heralds-mode)
      (define-key map (kbd "C-c v d") #'skg-view-diff-mode)
      (define-key map (kbd "C-c v h") ;; h as in heralds. m is taken by magit.
                  #'skg-view-metadata)
      (define-key map (kbd "C-c v o") #'skg-view-org-ancestry)
      (define-key map (kbd "C-c v m") #'skg-view-magit)
      (define-key map (kbd "C-c v s") #'skg-view-stack))
    (progn;; local changes to view (and possibly interpretation
      (define-key map (kbd "C-c l a") #'skg-local-aliases)
      (define-key map (kbd "C-c l c") #'skg-local-containerward)
      (define-key map (kbd "C-c l s") #'skg-local-sourceward)
      (define-key map (kbd "C-c l d") #'skg-local-definitive)
      (define-key map (kbd "C-c l i") #'skg-local-indefinitive)
      (define-key map (kbd "C-c l <backspace>") #'skg-local-delete))
    (progn;; id navigation and stack
      (define-key map (kbd "C-c i n") #'skg-id-next)
      (define-key map (kbd "C-c i p") #'skg-id-prev)
      (define-key map (kbd "C-c i u") #'skg-id-push-to-stack)
      (define-key map (kbd "C-c i o") #'skg-id-pop-from-stack))
    map )
  "Keymap for `skg-content-view-mode'.")

(skg-alias skg-save skg-request-save-buffer)
(skg-alias skg-local-aliases         skg-request-aliases-view)
(skg-alias skg-local-containerward   skg-request-containerward-view)
(skg-alias skg-local-definitive      skg-request-definitive-view)
(skg-alias skg-local-link-sourceward
           ;; For if someone forgets "source" but remembers "link".
           skg-request-sourceward-view)
(skg-alias skg-local-sourceward      skg-request-sourceward-view)
(skg-alias skg-view-from-node-id     skg-request-single-root-content-view-from-id)
(skg-alias skg-view-heralds-mode     heralds-minor-mode)
(skg-alias skg-view-metadata         skg-edit-metadata)

(provide 'skg-user-facing-aliases)
