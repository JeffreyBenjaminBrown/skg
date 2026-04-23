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
(skg-alias skg-show-aliases          skg-request-aliases-view)
(skg-alias skg-show-containerward    skg-request-containerward-view)
(skg-alias skg-show-sourceward       skg-request-sourceward-view)
(skg-alias skg-show-link-sourceward
           ;; For if someone forgets "source" but remembers "link".
           skg-request-sourceward-view)
(skg-alias skg-make-definitive       skg-request-definitive-view)
(skg-alias skg-view-heralds-mode     heralds-minor-mode)
(skg-alias skg-view-metadata         skg-edit-metadata)

;;
;; Keymaps
;;

(progn ;; Global
  (global-set-key (kbd "C-c f RET") #'skg-search)
  (global-set-key (kbd "C-c f i")   #'skg-search-interactive))

(with-eval-after-load 'magit ;; Magit
  (define-key magit-mode-map (kbd "C-c g RET") #'skg-goto)
  (define-key magit-mode-map (kbd "C-c G RET") #'skg-goto-and-close-this)
  (define-key magit-mode-map (kbd "C-c o i")   #'skg-paste-id)
  (define-key magit-mode-map (kbd "C-c o l")   #'skg-paste-link)
  (define-key magit-mode-map (kbd "C-c o I")   #'skg-pop-id)
  (define-key magit-mode-map (kbd "C-c o L")   #'skg-pop-link))

(defvar skg-content-view-mode-map ;; Content view keymap
  (let (( map (make-sparse-keymap) ))
    (progn;; save
      (define-key map (kbd "C-x C-s") #'skg-request-save-buffer))
    (progn;; delete (prefix arg => recursive)
      (define-key map (kbd "C-c <backspace>") #'skg-delete))
    (progn;; text search
      (define-key map (kbd "C-c f RET") #'skg-search)
      (define-key map (kbd "C-c f i")   #'skg-search-interactive))
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
      (define-key map (kbd "C-c s a") #'skg-show-aliases)
      (define-key map (kbd "C-c s c") #'skg-show-containerward)
      (define-key map (kbd "C-c s n") #'skg-show-org-ancestry)
      (define-key map (kbd "C-c s s") #'skg-show-sourceward)
      (define-key map (kbd "C-c s t") #'skg-show-stats))
    (progn;; properties
      (define-key map (kbd "C-c p d") #'skg-make-definitive)
      (define-key map (kbd "C-c p i") #'skg-make-indefinitive))
    (progn;; view (buffer-level view state)
      (define-key map (kbd "C-c v d") #'skg-view-diff-mode)
      (define-key map (kbd "C-c v e") #'skg-view-new-empty)
      (define-key map (kbd "C-c v h") #'skg-view-heralds-mode)
      (define-key map (kbd "C-c v m") #'skg-view-metadata)
      (define-key map (kbd "C-c v s") #'skg-view-id-stack))
    (progn;; id navigation (moving among IDs in the current buffer)
      (define-key map (kbd "C-c i n") #'skg-id-next)
      (define-key map (kbd "C-c i p") #'skg-id-prev)
      (define-key map (kbd "C-c i u") #'skg-id-push))
    (progn;; copy-from / pop-from the id stack
      (define-key map (kbd "C-c o i") #'skg-paste-id)
      (define-key map (kbd "C-c o l") #'skg-paste-link)
      (define-key map (kbd "C-c o I") #'skg-pop-id)
      (define-key map (kbd "C-c o L") #'skg-pop-link))
    map )
  "Keymap for `skg-content-view-mode'.")

(defvar skg-id-stack-mode-map ;; ID stack keymap
  (let (( map (make-sparse-keymap) ))
    (define-key map (kbd "C-x C-s") #'skg--save-id-stack-buffer)
    map )
  "Keymap for `skg-id-stack-mode'.")

(defvar skg-sexp-edit-mode-map ;; Sexp edit keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'skg-sexp-edit--commit)
    (define-key map (kbd "S-<left>") #'skg-sexp-edit-cycle-left)
    (define-key map (kbd "S-<right>") #'skg-sexp-edit-cycle-right)
    map)
  "Keymap for skg-sexp-edit-mode.")

(provide 'skg-keymaps-and-aliases)
