;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: All skg keybindings and user-facing command aliases.

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

;;
;; Aliases
;;

(skg-alias skg-save                  skg-request-save-buffer)
(skg-alias skg-reveal-aliases          skg-request-aliases-view)
(skg-alias skg-reveal-containerward    skg-request-containerward-view)
(skg-alias skg-reveal-sourceward       skg-request-sourceward-view)
(skg-alias skg-reveal-link-sourceward
           ;; For if someone forgets "source" but remembers "link".
           skg-request-sourceward-view)
(skg-alias skg-set-definitive       skg-request-definitive-view)
(skg-alias skg-view-heralds-mode     heralds-minor-mode)
(skg-alias skg-view-metadata         skg-edit-metadata)

;;
;; Keymaps
;;

(progn ;; Global
  (global-set-key (kbd "C-c f RET") #'skg-search)
  (global-set-key (kbd "C-c f i")   #'skg-search-interactive)
  (global-set-key (kbd "C-c f l")   #'skg-search-make-link))

(with-eval-after-load 'magit ;; Magit
  ;; Popping to the id stack would make no sense here,
  ;; because magit buffers cannot be written to.
  ;; Pushing to it could be done from magit, but would be tricky,
  ;; requiring extracting titles from overlays.
  (define-key magit-mode-map (kbd "C-c g RET") #'skg-goto)
  (define-key magit-mode-map (kbd "C-c G RET") #'skg-goto-and-close-this)
  (define-key magit-mode-map (kbd "C-c i n")   #'skg-id-next)
  (define-key magit-mode-map (kbd "C-c i p")   #'skg-id-prev))

(progn ;; Minibuffer
  (define-key minibuffer-local-map (kbd "C-c o i") #'skg-paste-id)
  (define-key minibuffer-local-map (kbd "C-c o l") #'skg-paste-link)
  (define-key minibuffer-local-map (kbd "C-c O i") #'skg-pop-id)
  (define-key minibuffer-local-map (kbd "C-c O l") #'skg-pop-link))

(with-eval-after-load 'shell ;; M-x shell
  ;; A shell buffer is writable (you can type at the prompt), so
  ;; insert-from-stack ops make sense here, just like in the
  ;; minibuffer.
  (define-key shell-mode-map (kbd "C-c o i") #'skg-paste-id)
  (define-key shell-mode-map (kbd "C-c o l") #'skg-paste-link)
  (define-key shell-mode-map (kbd "C-c O i") #'skg-pop-id)
  (define-key shell-mode-map (kbd "C-c O l") #'skg-pop-link))

;; Pattern used for every mode-map below: a `defvar' declares the
;; variable with an empty `make-sparse-keymap', and a separate
;; top-level form clears-and-repopulates it. Clearing is done by
;; `setcdr … nil' (mutating the keymap list's tail), which preserves
;; the keymap object identity captured by `define-derived-mode' /
;; `define-minor-mode' while ensuring that new bindings added to
;; this file take effect on reload. A naive `(defvar MAP (let ((m
;; (make-sparse-keymap))) …bindings… m))' does NOT re-run its
;; initialiser on a second load, so edits to the bindings would
;; silently do nothing.

(defvar skg-file-minor-mode-map (make-sparse-keymap)
  ;; The minor mode itself is defined in skg-file-minor-mode.el.
  ;; Ops to push to the id stack are omitted, like in magit,
  ;; because associating a title with each ID is tricky.
  "Keymap for `skg-file-minor-mode'.")

(let ((map skg-file-minor-mode-map))
  (setcdr map nil)
  (define-key map (kbd "C-c g RET") #'skg-goto)
  (define-key map (kbd "C-c G RET") #'skg-goto-and-close-this)
  (define-key map (kbd "C-c i n")   #'skg-id-next)
  (define-key map (kbd "C-c i p")   #'skg-id-prev)
  (define-key map (kbd "C-c o i")   #'skg-paste-id)
  (define-key map (kbd "C-c o l")   #'skg-paste-link)
  (define-key map (kbd "C-c O i")   #'skg-pop-id)
  (define-key map (kbd "C-c O l")   #'skg-pop-link))

(defvar skg-content-view-mode-map (make-sparse-keymap)
  "Keymap for `skg-content-view-mode'.")

(let ((map skg-content-view-mode-map))
  (setcdr map nil)
  (progn;; save
    (define-key map (kbd "C-x C-s") #'skg-request-save-buffer))
  (progn;; delete (prefix arg => recursive)
    (define-key map (kbd "C-c <backspace>") #'skg-delete))
  (progn;; text search
    (define-key map (kbd "C-c f RET") #'skg-search)
    (define-key map (kbd "C-c f i")   #'skg-search-interactive)
    (define-key map (kbd "C-c f l")   #'skg-search-make-link))
  (progn;; goto. Capital-G variants kill the buffer they were called from.
    (define-key map (kbd "C-c g RET") #'skg-goto)
    (define-key map (kbd "C-c G RET") #'skg-goto-and-close-this)
    (define-key map (kbd "C-c g i")   #'skg-goto-by-id)
    (define-key map (kbd "C-c G i")   #'skg-goto-by-id-and-close-this)
    (define-key map (kbd "C-c g m")   #'skg-goto-in-magit)
    (define-key map (kbd "C-c G m")   #'skg-goto-in-magit-and-close-this)
    (define-key map (kbd "C-c g M")   #'skg-goto-in-magit-parent)
    (define-key map (kbd "C-c G M")   #'skg-goto-in-magit-parent-and-close-this))
  (progn;; show (requests that server render a local change to the view)
    (define-key map (kbd "C-c r a") #'skg-reveal-aliases)
    (define-key map (kbd "C-c r c") #'skg-reveal-containerward)
    (define-key map (kbd "C-c r s") #'skg-reveal-sourceward))
  (progn;; properties
    (define-key map (kbd "C-c s d") #'skg-set-definitive)
    (define-key map (kbd "C-c s i") #'skg-set-indefinitive)
    (define-key map (kbd "C-c s s") #'skg-set-source)
    (define-key map (kbd "C-c s S") #'skg-set-source-recursive))
  (progn;; graph modifications
    (define-key map (kbd "C-c m c") #'skg-replace-link-with-content)
    (define-key map (kbd "C-c m l") #'skg-replace-content-with-link))
  (progn;; view (buffer-level view state)
    (define-key map (kbd "C-c v d") #'skg-view-diff-mode)
    (define-key map (kbd "C-c v e") #'skg-view-new-empty)
    (define-key map (kbd "C-c v h") #'skg-view-heralds-mode)
    (define-key map (kbd "C-c v m") #'skg-view-metadata)
    (define-key map (kbd "C-c v o") #'skg-view-org-ancestry)
    (define-key map (kbd "C-c v r") #'skg-readable-ids-mode)
    (define-key map (kbd "C-c v s") #'skg-view-id-stack)
    (define-key map (kbd "C-c v w") #'skg-view-without-metadata) )
  (progn;; id navigation (moving among IDs in the current buffer)
    (define-key map (kbd "C-c i n") #'skg-id-next)
    (define-key map (kbd "C-c i p") #'skg-id-prev)
  (progn;; id stack operations (push and pop, u and o)
    (define-key map (kbd "C-c u") #'skg-id-push))
    (define-key map (kbd "C-c o i") #'skg-paste-id)
    (define-key map (kbd "C-c o l") #'skg-paste-link)
    (define-key map (kbd "C-c O i") #'skg-pop-id)
    (define-key map (kbd "C-c O l") #'skg-pop-link))
  (progn;; git add ops (only fire if the .skg file is not yet in HEAD)
    (define-key map (kbd "C-c t A") #'skg-git-add-if-new-recursive)
    (define-key map (kbd "C-c t a") #'skg-git-add-if-new-recursive-preview)))

(defvar skg-id-stack-mode-map (make-sparse-keymap)
  "Keymap for `skg-id-stack-mode'.")

(let ((map skg-id-stack-mode-map))
  (setcdr map nil)
  (define-key map (kbd "C-x C-s") #'skg--save-id-stack-buffer))

(defvar skg-sexp-edit-mode-map (make-sparse-keymap)
  "Keymap for skg-sexp-edit-mode.")

(let ((map skg-sexp-edit-mode-map))
  (setcdr map nil)
  (define-key map (kbd "C-c C-c")   #'skg-sexp-edit--commit)
  (define-key map (kbd "S-<left>")  #'skg-sexp-edit-cycle-left)
  (define-key map (kbd "S-<right>") #'skg-sexp-edit-cycle-right))

(provide 'skg-keymaps-and-aliases)
