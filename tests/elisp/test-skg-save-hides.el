;;; test-skg-save-hides.el --- A save says what it hid -*- lexical-binding: t; -*-

;; The server infers hides from what left a forker, so no edit
;; shows one. A save's answer names each in '(hides ...)', and the
;; client shows them in the echo area, below whatever the save showed
;; already ('docs/api-and-formats.org', "Save buffer").

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-request-save)

(ert-deftest test-skg-a-save-shows-what-it-hid ()
  (let ((shown '()))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) shown)))
              ((symbol-function 'current-message)
               (lambda () (car shown))))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg node (gnid g)) g\n\") (errors ()) (warnings ())"
                 " (hides (\"Hides 'm' from 'g', as content of 'j'.\""
                 " \"Hides nothing: 'y' left 'g', but it is no content of 'j', nor a mirror of any.\")))")))
      (should (equal (car shown)
                     (concat "Buffer updated with processed content from Rust\n"
                             "Hides 'm' from 'g', as content of 'j'.\n"
                             "Hides nothing: 'y' left 'g', but it is no content of 'j', nor a mirror of any.")))
      (setq shown '())
      (with-temp-buffer
        (skg-handle-save-sexp
         "((content \"* (skg node (gnid g)) g\n\") (errors ()) (warnings ()))"))
      (should (equal shown
                     '("Buffer updated with processed content from Rust"))))))

(ert-deftest test-skg-save-hides-message-without-an-earlier-message ()
  (should (equal (skg--save-hides-message '("Hides 'm' from 'g'.") nil)
                 "Hides 'm' from 'g'."))
  (should (equal (skg--save-hides-message '("Hides 'm' from 'g'.") "")
                 "Hides 'm' from 'g'.")))

;;; test-skg-save-hides.el ends here
