;;; test-skg-metadata-editing.el --- Tests for skg metadata editing functions

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-metadata)
(require 'skg-sexpr-edit)

(ert-deftest test-sexp-edit-herald-hints-appear-as-overlays ()
  "The metadata view appends, per field/value headline, the herald it
produces -- as an overlay (never buffer text), colored tokens inside
plain parens. Fields producing no herald get none."
  (skg-test-install-herald-rules)
  (with-temp-buffer
    (insert "* skg\n** node\n** skgRepos\n*** nodeRepo\n**** main\n** writeProtected\n"
            "** noClaim\n*** true\n")
    (org-mode)
    (skg-sexp-edit--decorate-with-heralds)
    (let ((hint-on-line
           (lambda (needle)
             (goto-char (point-min))
             (search-forward needle)
             (let ((ov (seq-find
                        (lambda (o)
                          (overlay-get o 'skg-sexp-edit-herald))
                        ;; 1+: an empty overlay sitting exactly at
                        ;; line-end is excluded by overlays-in when
                        ;; the range stops there.
                        (overlays-in (line-beginning-position)
                                     (min (point-max)
                                          (1+ (line-end-position)))))))
               (and ov (substring-no-properties
                        (overlay-get ov 'after-string)))))))
      (should (equal (funcall hint-on-line "** writeProtected") " (\u262e)"))
      (should-not (funcall hint-on-line "** noClaim"))
      (should-not (funcall hint-on-line "**** main"))
      (should-not (funcall hint-on-line "*** nodeRepo"))
      ;; The buffer text is untouched: applying must not see hints.
      (should-not (string-match-p "(\u262e)" (buffer-string))))))

(provide 'test-skg-metadata-editing)
