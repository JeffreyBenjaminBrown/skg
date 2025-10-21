;;; test-skg-insert-nested-sexp.el --- Tests for skg-merge-into-nested-s-exp

(add-to-list
 'load-path
 (expand-file-name "../../elisp"
                   (file-name-directory load-file-name)))

(require 'ert)
(require 'skg-metadata)

(ert-deftest test-merge-nested-sexp-flat ()
  "Test flat insertion: (skg x) + (skg y) -> (skg x y)."
  (let ((host '(skg x))
        (guest '(skg y))
        (expected '(skg x y)))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-nested-destination-exists ()
  "Test nested insertion when destination exists:
(skg (code x)) + (skg (code y)) -> (skg (code x y))."
  (let ((host '(skg (code x)))
        (guest '(skg (code y)))
        (expected '(skg (code x y))))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-nested-destination-does-not-exist ()
  "Test nested insertion when destination does not exist:
(skg (view x)) + (skg (code y)) -> (skg (view x) (code y))."
  (let ((host '(skg (view x)))
        (guest '(skg (code y)))
        (expected '(skg (view x) (code y))))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-double-nested-destination-does-not-exist ()
  "Test double-nested insertion when destination does not exist:
(skg feathers) + (skg (code (something y))) -> (skg feathers (code (something y)))."
  (let ((host '(skg feathers))
        (guest '(skg (code (something y))))
        (expected '(skg feathers (code (something y)))))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-flat-multiple-leaves ()
  "Test flat insertion with multiple leaves:
(skg x) + (skg y (k v)) -> (skg x y (k v))."
  (let ((host '(skg x))
        (guest '(skg y (k v)))
        (expected '(skg x y (k v))))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-nested-destinations-exist ()
  "Test nested insertion when destinations exist:
(skg (code x)) + (skg (code y) (k v)) -> (skg (code x y) (k v))."
  (let ((host '(skg (code x)))
        (guest '(skg (code y) (k v)))
        (expected '(skg (code x y) (k v))))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-nested-destinations-exist-somewhat ()
  "Test nested insertion when destinations exist somewhat:
(skg (view x) (code) (a e)) + (skg (code (inner y)) (a (b c) d))
  -> (skg (view x) (code (inner y)) (a e (b c) d))."
  (let ((host '(skg (view x) (code) (a e)))
        (guest '(skg (code (inner y)) (a (b c) d)))
        (expected '(skg (view x) (code (inner y)) (a e (b c) d))))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-avoids-duplicate-flat ()
  "Test that merging avoids duplicating flat elements:
(skg x y) + (skg y z) -> (skg x y z)."
  (let ((host '(skg x y))
        (guest '(skg y z))
        (expected '(skg x y z)))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-avoids-duplicate-nested ()
  "Test that merging avoids duplicating nested elements:
(skg (code x y)) + (skg (code y z)) -> (skg (code x y z))."
  (let ((host '(skg (code x y)))
        (guest '(skg (code y z)))
        (expected '(skg (code x y z))))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-avoids-duplicate-mixed ()
  "Test that merging avoids duplicates with mixed content:
(skg (code x) y) + (skg (code x) y z) -> (skg (code x) y z)."
  (let ((host '(skg (code x) y))
        (guest '(skg (code x) y z))
        (expected '(skg (code x) y z)))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-prior-order-dominates ()
  "Test that host order dominates when both have same nested keys:
(skg (a a1) (b b1)) + (skg (b b2) (a a2) c) -> (skg (a a1 a2) (b b1 b2) c)."
  (let ((host '(skg (a a1) (b b1)))
        (guest '(skg (b b2) (a a2) c))
        (expected '(skg (a a1 a2) (b b1 b2) c)))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-nil-host-empty ()
  "Test merging with nil host and empty guest:
nil + (skg) -> (skg)."
  (let ((host nil)
        (guest '(skg))
        (expected '(skg)))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(ert-deftest test-merge-nested-sexp-nil-host-with-content ()
  "Test merging with nil host and non-empty guest:
nil + (skg (a b) c) -> (skg (a b) c)."
  (let ((host nil)
        (guest '(skg (a b) c))
        (expected '(skg (a b) c)))
    (should (equal (skg-merge-into-nested-s-exp host guest)
                   expected))))

(provide 'test-skg-insert-nested-sexp)
;;; test-skg-insert-nested-sexp.el ends here
