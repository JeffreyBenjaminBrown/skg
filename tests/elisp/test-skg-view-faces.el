;;; test-skg-view-faces.el --- Skg dictates the faces in its views  -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-buffer)

(ert-deftest test-skg-view-faces-remap-default-and-org-faces ()
  "A view remaps, buffer-locally, the default face and the Org faces it
uses to the view_faces of shared/herald-styles.json, leaving other
buffers alone."
  (with-temp-buffer
    (skg-content-view-mode)
    (let ((default-remap (cadr (assq 'default face-remapping-alist)))
          (headline-remap (cadr (assq 'org-level-1 face-remapping-alist))))
      (should (equal (plist-get default-remap :foreground) "white"))
      (should (equal (plist-get default-remap :background) "black"))
      (should (eq (plist-get headline-remap :weight) 'bold))
      ;; a face with no background of its own gets the default's
      (should (equal (plist-get headline-remap :background) "black"))
      (should (assq 'org-link face-remapping-alist))))
  (with-temp-buffer
    (org-mode)
    (should-not (assq 'default face-remapping-alist))))
