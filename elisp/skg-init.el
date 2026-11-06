;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Load everything the skg client needs.

(let ;; Add project directories to load-path
    ((project-root
      (file-name-directory
       (directory-file-name
        (file-name-directory
         (or load-file-name buffer-file-name)) )) ))
  (add-to-list
   'load-path
   (expand-file-name "elisp" project-root))
  (add-to-list
   'load-path
   (expand-file-name "elisp/skg-sexpr" project-root))
  (add-to-list
   'load-path
   (expand-file-name "elisp/skg-request-save" project-root))
  (add-to-list
   'load-path
   (expand-file-name "elisp/skg-sexpr-edit" project-root)) )

(when (featurep 'native-compile)
  ;; Jeff keeps all compilation inside Docker (bash/compile-elisp.sh
  ;; writes the .elc). Without this, an Emacs built with native compilation
  ;; would compile every loaded .elc to .eln on the host in the background,
  ;; printing cross-file "not known to be defined" warnings each time.
  (or (require 'comp-run nil t) ;; Emacs 30+ defines the deny list here,
      (require 'comp nil t))    ;; Emacs 29 here.
  (when (boundp 'native-comp-jit-compilation-deny-list)
    (add-to-list
     'native-comp-jit-compilation-deny-list
     (concat "\\`" (regexp-quote
                     (file-name-directory
                      (or load-file-name buffer-file-name)) )) )) )

(let ;; bash/compile-elisp.sh writes a .elc beside each .el, and agents
    ;; edit .el files daily; so a .el newer than its .elc loads as
    ;; source, and a stale .elc never shadows it. A 'let', not a
    ;; 'setq', so the user's own value holds everywhere else.
    ((load-prefer-newer t))
  (require 'skg-log)
  (require 'skg-buffer)
  (require 'skg-client)
  (require 'skg-id-search)
  (require 'skg-request-text-search)
  (require 'skg-request-views)
  (require 'skg-sexpr-edit)
  (require 'skg-sexpr-search)
  (require 'skg-view-ancestry)
  (require 'skg-readable-ids)
  (require 'skg-file-minor-mode) )
