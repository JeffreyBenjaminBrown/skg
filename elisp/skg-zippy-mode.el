;;; skg-zippy-mode.el --- Type K to run what C-c K runs, in skg buffers  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-zippy-mode', in which typing runs skg commands instead of
;;; inserting text: in an skg buffer, the key sequence K runs what
;;; C-c K runs there. C-c z ('skg-zippy-mode') turns it on, z
;;; ('skg-zippy-exit') turns it off. Digits and - give a prefix
;;; argument; any other printable key zippy does not bind runs
;;; 'skg-zippy-unbound-key', which says so. The C-c keymaps it reads
;;; are 'skg-keymaps-and-aliases.el's.
;;;
;;; The mode is global: one on/off state for every skg buffer, the
;;; existing ones and those made later, and no effect anywhere else.
;;; An skg buffer is one in which a map of
;;; 'skg-zippy--modeMaps_withCcommands' is active. Each such buffer
;;; gets its own zippy keymap, built from the C-c keymap of its own
;;; kind (content view, report, .skg file), and installed through
;;; 'emulation-mode-map-alists' under the condition 'skg-zippy-mode';
;;; so toggling the global variable switches every skg buffer at once.
;;;
;;; A buffer's zippy keymap composes, in order of precedence:
;;; - 'skg-zippy--own-keys-map': z leaves zippy.
;;; - the C-c keymap of each skg mode map active in the buffer, the
;;;   very keymap object C-c is bound to there (not a copy), so a key
;;;   later bound under C-c is a zippy key too. Only skg's own C-c
;;;   keymap counts: 'skg-content-view-mode-map' inherits from
;;;   'org-mode-map', whose C-c keys stay out of zippy.
;;; - 'skg-zippy--printable-fallback-map': digits and - make a prefix
;;;   argument, and any other printable key just says it is unbound.
;;; Keys none of these bind (C-c itself, C-x, M-x, motion, C-g) work
;;; as usual. Control keys bound under C-c are zippy keys too: C-o
;;; runs what C-c C-o runs.
;;;
;;; Cursor colour: while zippy is on and a frame's selected window
;;; shows an skg buffer, that frame's cursor is 'skg-zippy-cursor-color'.
;;; Emacs keeps the cursor colour per frame, not per buffer, so while
;;; zippy is on, 'window-selection-change-functions' and
;;; 'window-buffer-change-functions' re-decide it for the frame whose
;;; selected window or its buffer changed. Before changing a frame's
;;; colour, zippy keeps the colour it had in a frame parameter, and
;;; restores exactly that when the frame no longer wants zippy's. If
;;; something else (a theme) changes the colour meanwhile, the new
;;; colour becomes the one to restore. Frames that cannot show a cursor
;;; colour (terminals) are left alone.
;;;
;;; Code:

(require 'skg-keymaps-and-aliases)

(defconst skg-zippy--modeMaps_withCcommands
  '( skg-content-view-mode-map
     skg-report-mode-map
     skg-file-minor-mode-map )
  "The skg mode maps that bind keys under C-c. A buffer in which one of
them is active is an skg buffer, for zippy's purposes.")

(defvar-local skg-zippy--emulation-alist nil
  "In an skg buffer, ((skg-zippy-mode . MAP)), MAP being that buffer's
zippy keymap; nil elsewhere. A member of `emulation-mode-map-alists',
so MAP is active wherever this is set, while `skg-zippy-mode' is on.")

(defcustom skg-zippy-cursor-color "green"
  "The cursor colour while zippy is on and the selected window shows
an skg buffer. Elsewhere, and when zippy is off, the cursor keeps the
colour it had before."
  :type 'color
  :group 'convenience)

(defvar skg-zippy--own-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map "z" #'skg-zippy-exit)
    map)
  "Zippy's keys that take precedence over the C-c keymap.")

(defvar skg-zippy--printable-fallback-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'skg-zippy-unbound-key)
    (dotimes (offset (- ?~ ?\s -1))
      (define-key map (string (+ ?\s offset)) #'skg-zippy-unbound-key))
    (dotimes (digit 10)
      (define-key map (number-to-string digit) #'digit-argument))
    (define-key map "-" #'negative-argument)
    map)
  "Zippy's keys beneath the C-c keymap: every printable ASCII key, and
by remapping `self-insert-command' every other one, says it is unbound,
except digits and -, which give a prefix argument as in `suppress-keymap'.")

(add-to-list 'emulation-mode-map-alists 'skg-zippy--emulation-alist)

(define-minor-mode skg-zippy-mode
  "Global minor mode in which typing a key sequence K in an skg buffer
runs what C-c K runs there, instead of inserting text. z leaves it.
Digits and - give a prefix argument, an unbound printable key does
nothing but say so, and every key C-c does not bind, C-c itself
included, works as usual. Buffers that are not skg buffers are
unaffected. See skg-zippy-mode.el."
  :global t
  :group 'convenience
  :lighter (:eval (skg-zippy--lighter-for-this-buffer))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (skg-zippy--update-this-buffer)) )
  (skg-zippy--install-or-remove-cursor-color-hooks)
  (dolist (frame (frame-list))
    (skg-zippy--refresh-cursor-color-of-frame frame)) )

(defun skg-zippy-exit ()
  "Leave `skg-zippy-mode', in every skg buffer."
  (interactive)
  (skg-zippy-mode -1))

(defun skg-zippy-unbound-key ()
  "Say that the key just typed is bound to nothing in zippy mode.
Zippy binds this instead of inserting a printable key."
  (interactive)
  (message "Zippy: %s is unbound here (z leaves zippy)"
           (key-description (this-single-command-keys)) ))

(defun skg-zippy--lighter-for-this-buffer ()
  "The mode-line lighter: \" Zippy\" in an skg buffer, nothing elsewhere."
  (if skg-zippy--emulation-alist " Zippy" ""))

(defun skg-zippy--update-this-buffer ()
  "Make this buffer's zippy keymap agree with
`skg-zippy-mode' and with whether this is an skg buffer. Run when the
mode toggles, and from the skg modes' hooks, so a buffer that becomes
an skg buffer while zippy is on starts in zippy."
  (let ((Cc-maps (skg-zippy--Cc-maps-of-this-buffer)))
    (if (and skg-zippy-mode Cc-maps)
        (progn
          (setq skg-zippy--emulation-alist
                (list (cons 'skg-zippy-mode
                            (skg-zippy--keymap-composing Cc-maps)) )))
      (setq skg-zippy--emulation-alist nil))
    (dolist (window (get-buffer-window-list nil nil t))
      (skg-zippy--refresh-cursor-color-of-frame
       (window-frame window)) )))

(defun skg-zippy--Cc-maps-of-this-buffer ()
  "The C-c keymaps of the skg mode maps active in this buffer, in order
of precedence; nil if this is not an skg buffer."
  (let ((skg-mode-maps
         (mapcar #'symbol-value skg-zippy--modeMaps_withCcommands)))
    (delq nil
          (mapcar
           (lambda (active-map)
             (and (memq active-map skg-mode-maps)
                  (skg-zippy--own-Cc-map-of active-map)) )
           (current-active-maps)) )))

(defun skg-zippy--own-Cc-map-of (mode-map)
  "The keymap MODE-MAP itself binds C-c to, or nil. Unlike `lookup-key',
this ignores MODE-MAP's parent: a derived mode's map inherits its
parent's (here `org-mode-map'), whose C-c keys zippy leaves out."
  (let ((Cc-map nil))
    (map-keymap-internal
     (lambda (event binding)
       (when (and (eq event ?\C-c) (keymapp binding))
         (setq Cc-map binding)) )
     mode-map)
    Cc-map))

(defun skg-zippy--keymap-composing (Cc-maps)
  "A zippy keymap: its own keys over CC-MAPS over the printable fallback.
The C-c keymaps are included as themselves, so later bindings in them
show through."
  (make-composed-keymap
   (append (list skg-zippy--own-keys-map)
           Cc-maps
           (list skg-zippy--printable-fallback-map)) ))

(defun skg-zippy--install-or-remove-cursor-color-hooks ()
  "While zippy is on, run the cursor colour refresh when a frame's
selected window or the buffer of one of its windows changes, and when a
theme is enabled or disabled (those that exist in this Emacs). Off,
remove them all."
  (dolist (hook '( window-selection-change-functions
                   window-buffer-change-functions
                   enable-theme-functions
                   disable-theme-functions ))
    (if skg-zippy-mode
        (add-hook hook #'skg-zippy--refresh-cursor-color-after-change)
      (remove-hook hook #'skg-zippy--refresh-cursor-color-after-change)) ))

(defun skg-zippy--refresh-cursor-color-after-change (&optional frame-or-theme)
  "The hook function: refresh FRAME-OR-THEME's frame if it is one
(the window hooks pass a frame), else (a theme was enabled or disabled)
every frame."
  (if (framep frame-or-theme)
      (skg-zippy--refresh-cursor-color-of-frame frame-or-theme)
    (dolist (frame (frame-list))
      (skg-zippy--refresh-cursor-color-of-frame frame)) ))

(defun skg-zippy--frame-wants-zippy-cursor-p (frame)
  "Whether FRAME's cursor should be zippy's colour: zippy is on and
FRAME's selected window shows an skg buffer (one with a zippy keymap).
While the minibuffer is selected, it does not."
  (and skg-zippy-mode
       (buffer-local-value 'skg-zippy--emulation-alist
                           (window-buffer (frame-selected-window frame)) )
       t))

(defun skg-zippy--refresh-cursor-color-of-frame (frame)
  "Make FRAME's cursor colour agree with whether it wants zippy's.
The colour before zippy's first change is kept in a frame parameter
and restored exactly. The colour zippy last set is kept in another, so
that a colour set by something else since (a theme) is noticed, and
taken as the one to restore."
  (when (and (frame-live-p frame)
             (skg-zippy--frame-can-show-cursor-color-p frame))
    (let* ((current-color (skg-zippy--cursor-color-of frame))
           (applied-color (frame-parameter
                           frame 'skg-zippy-applied-cursor-color))
           (saved-original (frame-parameter
                            frame 'skg-zippy-original-cursor-color)) )
      (when (and saved-original applied-color
                 (not (equal current-color applied-color)))
        (setq saved-original (list current-color))
        (set-frame-parameter frame 'skg-zippy-original-cursor-color
                             saved-original))
      (if (skg-zippy--frame-wants-zippy-cursor-p frame)
          (progn
            (unless saved-original
              (set-frame-parameter frame 'skg-zippy-original-cursor-color
                                   (list current-color)))
            (unless (equal current-color skg-zippy-cursor-color)
              (skg-zippy--set-cursor-color-of
               frame skg-zippy-cursor-color))
            (set-frame-parameter frame 'skg-zippy-applied-cursor-color
                                 skg-zippy-cursor-color))
        (when saved-original
          (skg-zippy--set-cursor-color-of frame (car saved-original))
          (set-frame-parameter frame 'skg-zippy-original-cursor-color nil)
          (set-frame-parameter frame 'skg-zippy-applied-cursor-color nil)) )) ))

(defun skg-zippy--frame-can-show-cursor-color-p (frame)
  "Whether FRAME can show a cursor colour: only graphical frames can."
  (display-graphic-p frame))

(defun skg-zippy--cursor-color-of (frame)
  "FRAME's cursor colour."
  (frame-parameter frame 'cursor-color))

(defun skg-zippy--set-cursor-color-of (frame color)
  "Set FRAME's cursor colour to COLOR."
  (set-frame-parameter frame 'cursor-color color))

(dolist (hook '( skg-content-view-mode-hook
                 skg-report-mode-hook
                 skg-file-minor-mode-hook ))
  (add-hook hook #'skg-zippy--update-this-buffer))

(defun skg-zippy-mode-unload-function ()
  "Leave zippy before `unload-feature' (as `skg-reload' does) removes it,
so that no buffer keeps its keymap, and no frame its cursor colour
or its hooks."
  (skg-zippy-mode -1)
  (setq emulation-mode-map-alists
        (delq 'skg-zippy--emulation-alist emulation-mode-map-alists))
  nil)

(provide 'skg-zippy-mode)
