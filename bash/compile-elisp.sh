#!/bin/bash
# USAGE: bash/compile-elisp.sh
#   No arguments; runs from any directory. bash/start-servers.sh runs it.
#
# Byte-compiles every .el under elisp/ (subdirectories too), writing each
# .elc beside its source, with the container's Emacs. It loads
# elisp/skg-init.el first, which puts elisp/'s load path in place and
# defines every macro the client uses. It deletes any .elc whose .el is
# gone. Warnings are printed and do not fail it; a file that fails to
# compile is named, loses its .elc, and makes the script exit 1.
#
# PITFALL: an Emacs that loads these .elc files must be at least the
# version that wrote them (the container's, 31.1). skg-init.el binds
# 'load-prefer-newer', so a .el edited after its .elc is loaded as source.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SKG_ELISP_DIR="$(cd "$SCRIPT_DIR/../elisp" && pwd)"
export SKG_ELISP_DIR

COMPILE_PROGRAM='
(let*
    ((elisp-dir (getenv "SKG_ELISP_DIR"))
     (load-prefer-newer t) ; a stale .elc must not stand in for its .el
     (compiled-count 0)
     (failed-files nil))
  (load (expand-file-name "skg-init.el" elisp-dir)
        nil t t) ; nosuffix: the source, never a stale skg-init.elc
  (dolist (elc-file (directory-files-recursively elisp-dir "\\.elc\\'"'"'"))
    (unless (file-exists-p (concat (file-name-sans-extension elc-file) ".el"))
      (delete-file elc-file)
      (message "Deleted %s: its .el is gone." elc-file)))
  (dolist (el-file (directory-files-recursively elisp-dir "\\.el\\'"'"'"))
    (when (and (file-regular-p el-file) ; not a dangling symlink
               (not (string-prefix-p ; nor an Emacs lock file
                     ".#" (file-name-nondirectory el-file))))
      (if (byte-compile-file el-file)
          (setq compiled-count (1+ compiled-count))
        (push el-file failed-files)
        (let ((elc-file (byte-compile-dest-file el-file)))
          (when (file-exists-p elc-file)
            (delete-file elc-file))))))
  (dolist (failed-file (reverse failed-files))
    (message "Compile error: %s" failed-file))
  (message "Compiled %d files; %d failed."
           compiled-count (length failed-files))
  (kill-emacs (if failed-files 1 0)))'

compile_output="$( emacs --batch -Q --eval "$COMPILE_PROGRAM" 2>&1 )"
compile_status=$?
warning_count="$( printf '%s\n' "$compile_output" | grep -c ': Warning:' )"
printf '%s\n' "$compile_output"
echo "compile-elisp.sh: $warning_count warnings; exit status $compile_status."
exit $compile_status
