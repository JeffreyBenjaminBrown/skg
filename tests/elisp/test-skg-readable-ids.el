;;; test-skg-readable-ids.el --- Tests for skg-readable-ids-mode.

(defconst test-skg-readable-ids--this-dir
  (file-name-directory load-file-name)
  "Captured at load time; `load-file-name' is nil inside ERT bodies.")

(load-file (expand-file-name "../../elisp/skg-init.el"
                             test-skg-readable-ids--this-dir))
(require 'ert)

(defconst test-skg-readable-ids--id-a
  "11111111-1111-4111-8111-111111111111")

(defconst test-skg-readable-ids--id-b
  "22222222-2222-4222-8222-222222222222")

(defun test-skg-readable-ids--titles-payload (id title)
  (format "((response-type titles-by-ids) (content ((%S . %S))))"
          id title))

(defun test-skg-readable-ids--after-string-count ()
  (let ((count 0))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'skg-magit-title)
                 (overlay-get ov 'after-string))
        (setq count (1+ count))))
    count))

(ert-deftest test-skg-readable-ids-later-response-survives-earlier-response ()
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0)
        (skg-lp--buf (unibyte-string))
        (skg-lp--bytes-left nil)
        (skg-readable-ids--pending-title-requests nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                 (lambda () 'fake-proc))
                ((symbol-function 'process-send-string)
                 (lambda (_proc _string) nil)))
        (insert test-skg-readable-ids--id-a)
        (setq skg-readable-ids--positions
              (skg-readable-ids--collect-ids))
        (setq skg-readable-ids--generation 1)
        (skg-readable-ids--request-titles
         (list test-skg-readable-ids--id-a)
         skg-readable-ids--generation
         (current-buffer))
        (erase-buffer)
        (insert test-skg-readable-ids--id-b)
        (setq skg-readable-ids--positions
              (skg-readable-ids--collect-ids))
        (setq skg-readable-ids--generation 2)
        (skg-readable-ids--request-titles
         (list test-skg-readable-ids--id-b)
         skg-readable-ids--generation
         (current-buffer))
        (skg-lp--dispatch-by-type
         nil
         (test-skg-readable-ids--titles-payload
          test-skg-readable-ids--id-a
          "Title A"))
        (skg-lp--dispatch-by-type
         nil
         (test-skg-readable-ids--titles-payload
          test-skg-readable-ids--id-b
          "Title B"))
        (should (= 1 (test-skg-readable-ids--after-string-count)))) )))
