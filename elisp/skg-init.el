;;; skg-init.el --- Load the skg Emacs client; index of its files  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-reload' ('skg-reload.el') loads this file, as 'docs/setup.org'
;;; starts the client; it puts 'elisp/' and its subfolders on the load
;;; path and requires the client's top files, which require the rest.
;;; 'skg-client-init' ('skg-client.el') then connects to the server. Read this list
;;; first to find a file; each file's own Commentary says more, and a
;;; file with a Neovim twin has the twin's name ('docs/nvim-client-
;;; deviations.org', "The two clients' names match").
;;;
;;; Starting, connecting, and the plumbing every request uses:
;;; - 'skg-client.el': the TCP connection to the server; the require hub.
;;; - 'skg-config.el': read skgconfig.toml.
;;; - 'skg-picker.el': the minibuffer pickers for a skgrepo or a rung.
;;; - 'skg-state.el': session-wide state: connection, handlers.
;;; - 'skg-length-prefix.el': split server replies, dispatch each by type.
;;; - 'skg-keymaps-and-aliases.el': every key binding and M-x alias.
;;; - 'skg-log.el': the debugging log, off by default.
;;; - 'skg-shared.el': read the JSON tables in 'shared/'.
;;; - 'skg-reload.el': reload the client from disk, without a restart.
;;; - 'skg-request-verify-connection.el': ping the server.
;;; - 'skg-request-rebuild-session-lifetime-stores.el': rebuild the
;;;   graph and the index.
;;; - 'skg-request-server-warnings.el': the server's startup warnings.
;;; - 'skg-repo-files-reloaded.el': files changed on disk: report, redraw.
;;; - 'skg-request-repo-sets.el': query and switch the skgrepo restriction.
;;; - 'skg-test-utils.el': for tests only: load path and fixtures.
;;;
;;; Opening views, moving and searching:
;;; - 'skg-buffer.el': the view buffer: its mode, state, opening, closing.
;;; - 'skg-request-single-root-content-view.el': open a view of one node.
;;; - 'skg-view-new-empty.el': open a blank content view.
;;; - 'skg-request-views.el': view requests: folders, editability, forks.
;;; - 'skg-request-perspective.el': cycle the perspective of a view.
;;; - 'skg-request-rerender-all-views.el': the rerender stream.
;;; - 'skg-view-ancestry.el': show the outline ancestry of a headline.
;;; - 'skg-push-ancestry.el': push the ancestry at point to the linkstack.
;;; - 'skg-id-search.el': find and visit the ID near point.
;;; - 'skg-linkstack.el': push, paste and pop IDs and links; view
;;;   the stack.
;;; - 'skg-request-text-search.el': text search and its results buffer.
;;; - 'skg-search-make-link.el': search, then insert a link to a hit.
;;; - 'skg-request-file-path.el': show a node's .skg file in magit.
;;; - 'skg-readable-ids.el': titles beside IDs in magit and .skg files.
;;; - 'skg-file-minor-mode.el': skg ID keys in buffers visiting .skg files.
;;; - 'skg-link-annotations.el': mark broken links; show home skgrepos.
;;; - 'skg-zippy-mode.el': type K to run what C-c K runs.
;;;
;;; Drawing metadata:
;;; - 'heralds-minor-mode.el': draw headline metadata as heralds.
;;; - 'skg-request-herald-rules.el': fetch the herald rule table.
;;; - 'skg-request-describe-heralds.el': explain a headline's heralds.
;;; - 'skg-fork-dedup.el': cover what a forks-folder member restates.
;;;
;;; Editing nodes and their metadata:
;;; - 'skg-metadata.el': read and edit a headline's (skg ...) metadata.
;;; - 'skg-modify-graph.el': swap a branch and a link; biggest branch.
;;; - 'skg-request-relRepo-info.el': choose a relationship's relRepo.
;;; - 'skg-request-flag-state.el': stage a node's noSearchMatching flag.
;;; - 'skg-request-receipt.el': write a read receipt for a mailed item.
;;; - 'skg-request-strip-body-whitespace.el': strip trailing body
;;;   whitespace.
;;; - 'skg-sexpr-edit.el': edit a headline's metadata as an org tree.
;;; - 'skg-sexpr-edit/skg-sexpr-cycling.el': S-left/S-right there.
;;; - 'skg-request-investigate-unknown-node.el': look up an unknown node.
;;; - 'skg-request-delete-references-to-absent-node.el': remove owned
;;;   references to it.
;;;
;;; Saving and redrawing:
;;; - 'skg-request-save.el': save a view and draw the server's answer.
;;; - 'skg-request-save/skg-save-confirmations.el': a save's
;;;   confirmation requests.
;;; - 'skg-request-save/skg-lock-buffers.el': lock views while a save
;;;   or rerender runs.
;;; - 'skg-redraw-delta.el': rewrite only the headlines a save changed.
;;; - 'skg-redraw-whole-view.el': redraw a view, keeping point and folds.
;;; - 'skg-org-fold.el': the body toggle, and folds across a save.
;;; - 'skg-messages.el': the server's errors and warnings in a buffer.
;;; - 'skg-recovery.el': archive a dirty view's edits for recovery.
;;;
;;; The diff view ('docs/diff-view.org'):
;;; - 'skg-request-diff-view.el': open the diff view's buffers.
;;; - 'skg-request-extend-diff-view.el': extend a diff-view buffer.
;;; - 'skg-change-facts.el': read a headline's change facts.
;;; - 'skg-processed.el': mark changes processed, and move between them.
;;; - 'skg-restore-requests.el': ask a save to restore what was removed.
;;; - 'skg-request-stage-and-commit.el': git-commit the changed nodes.
;;;
;;; Import and export:
;;; - 'skg-request-export-org.el': export skg data to plain .org files.
;;; - 'skg-request-import-md-and-org.el': import Markdown and Org files.
;;;
;;; S-expressions, the metadata's syntax ('skg-sexpr/'):
;;; - 'skg-sexpr-search.el': read sexps on a line, and values by path.
;;; - 'skg-compare-sexpr.el': whether one sexp holds another's paths.
;;; - 'skg-lens.el': transform sexps by declarative rules.
;;; - 'skg-sexpr-dsl-for-edits.el': a small DSL for editing sexps.
;;; - 'skg-sexpr-org-bijection.el': sexps to org text and back.
;;; - 'skg-metadata-fields.el': the fields a user may set on a headline.
;;;
;;; Not loaded: 'abandoned/' (dead code kept as reference) and
;;; 'experim/' (scratch). 'PITFALLs.org' holds the client's traps.
;;; A new file must join 'skg--reload-unloaded-features'
;;; ('skg-reload.el'), or 'tests/elisp/test-skg-reload.el' fails.
;;;
;;; Code:

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
  (require 'skg-linkstack)
  (require 'skg-push-ancestry)
  (require 'skg-request-text-search)
  (require 'skg-request-views)
  (require 'skg-sexpr-edit)
  (require 'skg-sexpr-search)
  (require 'skg-view-ancestry)
  (require 'skg-readable-ids)
  (require 'skg-file-minor-mode)
  (require 'skg-zippy-mode) )
