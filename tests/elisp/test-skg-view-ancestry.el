;;; test-skg-view-ancestry.el --- Tests for skg-view-ancestry -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'heralds-minor-mode)
(require 'skg-view-ancestry)

(defun test-skg-view-ancestry--run (source-shows-heralds)
  "Call `skg-view-ancestry' from 'e' in the docstring's example buffer,
whose `heralds-minor-mode' variable is SOURCE-SHOWS-HERALDS. Return
\(TEXT . HERALDS-ENABLED): the new buffer's text, and whether
`heralds-minor-mode' was turned on in it."
  (let ((enabled-in nil) (ancestry-buffer nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* a\n** b\n** c\n*** d\n*** e\n*** f\n")
          (setq-local heralds-minor-mode source-shows-heralds)
          (goto-char (point-min))
          (forward-line 4)
          (cl-letf (((symbol-function 'heralds-minor-mode)
                     (lambda (&optional _arg)
                       (setq enabled-in (current-buffer)))))
            (save-window-excursion
              (skg-view-ancestry)
              (setq ancestry-buffer (current-buffer))))
          (cons (with-current-buffer ancestry-buffer (buffer-string))
                (eq enabled-in ancestry-buffer)))
      (when (buffer-live-p ancestry-buffer)
        (kill-buffer ancestry-buffer)))))

(ert-deftest test-skg-view-ancestry-shows-only-the-ancestry ()
  (should (equal (car (test-skg-view-ancestry--run nil))
                 "* a\n** c\n*** e\n")))

(ert-deftest test-skg-view-ancestry-inherits-heralds ()
  "The ancestry buffer shows heralds exactly when its source does."
  (should (cdr (test-skg-view-ancestry--run t)))
  (should-not (cdr (test-skg-view-ancestry--run nil))))

(defun test-skg-view-ancestry--name-from (headline)
  "The name of the buffer `skg-view-ancestry' opens from the second
of two headlines, HEADLINE being that second one."
  (let ((ancestry-buffer nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* (skg node (vnid 01) (gnid p)) parent\n" headline "\n")
          (goto-char (point-min))
          (forward-line 1)
          (save-window-excursion
            (skg-view-ancestry)
            (setq ancestry-buffer (current-buffer)))
          (buffer-name ancestry-buffer))
      (when (buffer-live-p ancestry-buffer)
        (kill-buffer ancestry-buffer)))))

(ert-deftest test-skg-view-ancestry-buffer-name ()
  "The buffer is named by the headline's vnid, when its metadata has
one, and its title, never by the raw metadata."
  (should (equal (test-skg-view-ancestry--name-from
                  "** (skg node (vnid 0c) (gnid abc) (source main)) the title")
                 "*Ancestry (vnid 0c): the title*"))
  (should (equal (test-skg-view-ancestry--name-from
                  "** (skg node (gnid abc)) no vnid here")
                 "*Ancestry: no vnid here*"))
  (should (equal (test-skg-view-ancestry--name-from
                  "** a plain org headline")
                 "*Ancestry: a plain org headline*")))

(provide 'test-skg-view-ancestry)
