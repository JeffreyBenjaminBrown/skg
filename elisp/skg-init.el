;;; init.el --- Project-specific Emacs configuration

;; Add project directories to load-path
(let ((project-root
       (file-name-directory
        (directory-file-name
         (file-name-directory
          (or load-file-name buffer-file-name)) )) ))
  (add-to-list
   'load-path
   (expand-file-name "elisp" project-root)) )

(require 'skg-client)
(require 'skg-id-search)
