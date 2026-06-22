;;; test-skg-sexpr-search.el --- Tests for skg-sexpr-search

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-sexpr-search)

;;
;; Test text 1: ** (a (b c) d) hello
;; The sexp (a (b c) d) spans characters 4-14 (1-indexed Emacs positions)
;; Characters 1-14 should find the sexp, 15+ should error
;;

(defconst test-text-1 "** (a (b c) d) hello"
  "Test text with a complete sexp on a single line.")

(ert-deftest test-first-sexpr-on-line-from-beginning ()
  "skg-first-sexpr-on-line finds sexp from beginning of line."
  (with-temp-buffer
    (insert test-text-1)
    (goto-char (point-min))
    (should (equal (skg-first-sexpr-on-line) '(a (b c) d)))))

(ert-deftest test-first-sexpr-on-line-from-end ()
  "skg-first-sexpr-on-line finds sexp from end of line."
  (with-temp-buffer
    (insert test-text-1)
    (goto-char (point-max))
    (should (equal (skg-first-sexpr-on-line) '(a (b c) d)))))

(ert-deftest test-sexp-at-or-after-point-within-sexp ()
  "skg-sexp-at-or-after-point finds sexp from positions 1-14.
Tests position 1, position 14, and one random position in [2,13]."
  (with-temp-buffer
    (insert test-text-1)
    (let ((positions (list 1
                           14
                           (+ 2 (random 12)))))  ; 2-13
      (dolist (pos positions)
        (goto-char pos)
        (let ((result (condition-case err
                          (skg-sexp-at-or-after-point)
                        (error (ert-fail
                                (format "Unexpected error at position %d: %s"
                                        pos (error-message-string err)))))))
          (unless (equal result '(a (b c) d))
            (ert-fail (format "Wrong result at position %d: %S" pos result))))))))

(ert-deftest test-sexp-at-or-after-point-after-sexp ()
  "skg-sexp-at-or-after-point errors when point is beyond the sexp.
Tests position 15, position 20, and one random position in [16,19]."
  (with-temp-buffer
    (insert test-text-1)
    ;; Position 15-20 (after closing paren) should error
    (let ((positions (list 15
                           20
                           (+ 16 (random 4)))))  ; 16-19
      (dolist (pos positions)
        (goto-char pos)
        (let ((errored nil)
              (result nil))
          (condition-case nil
              (setq result (skg-sexp-at-or-after-point))
            (error (setq errored t)))
          (unless errored
            (ert-fail (format "Should have errored at position %d, got: %S"
                              pos result))))))))

;;
;; Test text 2: multi-line sexp that should error
;; Line 1: ** hello (a (b c)
;; Line 2: ** d)
;;

(defconst test-text-2 "** hello (a (b c)\n** d)"
  "Test text with a sexp spanning two lines.")

(ert-deftest test-first-sexpr-on-line-multiline-errors ()
  "skg-first-sexpr-on-line errors on multi-line sexp.
Tests position 1, position 17 (end of line 1), and one random in [2,16]."
  (with-temp-buffer
    (insert test-text-2)
    (goto-char (point-min))
    (let* ((line-1-end (line-end-position))  ; 17
           (positions (list 1
                            line-1-end
                            (+ 2 (random (- line-1-end 2))))))  ; 2 to line-1-end-1
      (dolist (pos positions)
        (goto-char pos)
        (let ((errored nil)
              (result nil))
          (condition-case nil
              (setq result (skg-first-sexpr-on-line))
            (error (setq errored t)))
          ;; Either it errors, or returns nil (no sexp found) - both are acceptable
          ;; What's NOT acceptable is returning a sexp
          (when (and (not errored) result)
            (ert-fail (format "Should have errored or returned nil at position %d, got: %S"
                              pos result))))))))

(ert-deftest test-sexp-at-or-after-point-multiline-errors ()
  "skg-sexp-at-or-after-point errors on multi-line sexp.
Tests position 1, position 17 (end of line 1), and one random in [2,16]."
  (with-temp-buffer
    (insert test-text-2)
    (goto-char (point-min))
    (let* ((line-1-end (line-end-position))  ; 17
           (positions (list 1
                            line-1-end
                            (+ 2 (random (- line-1-end 2))))))  ; 2 to line-1-end-1
      (dolist (pos positions)
        (goto-char pos)
        (let ((errored nil)
              (result nil))
          (condition-case nil
              (setq result (skg-sexp-at-or-after-point))
            (error (setq errored t)))
          (unless errored
            (ert-fail (format "Should have errored at position %d, got: %S"
                              pos result))))))))

;;
;; skg-strip-heralds-from-sexp: remove herald subfields where present,
;; but fabricate nothing (regression for the (node ...) fabrication bug).
;;

(ert-deftest test-strip-heralds-removes-herald-subfields ()
  "Heralds (the node-level birthHerald / rels display strings, and
sourceHerald inside viewStats) are removed; a viewStats emptied by the
removal is dropped, other data is kept."
  (should (equal
           (skg-strip-heralds-from-sexp
            '(skg (node (id x)
                        (birthHerald "aC")
                        (rels "3C")
                        (viewStats (sourceHerald ⌂:priv) cycle))))
           '(skg (node (id x) (viewStats cycle))))))

(ert-deftest test-strip-heralds-drops-stats-form-with-only-heralds ()
  "A graphStats/viewStats holding only herald subfields is dropped."
  (should (equal
           (skg-strip-heralds-from-sexp
            '(skg (node (id x) (viewStats (sourceHerald ⌂:priv)))))
           '(skg (node (id x))))))

(ert-deftest test-strip-heralds-does-not-fabricate-node ()
  "A non-ActiveNode sexp (no (node ...)) is returned unchanged, so it
never gains a fabricated (node ...) and never matches (skg (node))."
  (let ((stripped (skg-strip-heralds-from-sexp '(skg (id x)))))
    (should (equal stripped '(skg (id x))))
    (should-not (skg-sexp-subtree-p stripped '(skg (node))))))

(ert-deftest test-strip-heralds-activeNode-without-stats-unchanged ()
  "A ActiveNode lacking stats keeps exactly its fields -- no fabricated
graphStats/viewStats."
  (should (equal
           (skg-strip-heralds-from-sexp '(skg (node (id x) (source y))))
           '(skg (node (id x) (source y))))))

(ert-deftest test-strip-heralds-passes-through-non-skg ()
  "A sexp that does not start with skg is returned unchanged."
  (should (equal (skg-strip-heralds-from-sexp '(a (b c) d))
                 '(a (b c) d))))

(provide 'test-skg-sexpr-search)
