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

(defconst test-skg-readable-ids--id-inactive
  "33333333-3333-4333-8333-333333333333")

(defconst test-skg-readable-ids--id-active
  "44444444-4444-4444-8444-444444444444")

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

(defun test-skg-readable-ids--display-count ()
  (let ((count 0))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'skg-magit-title)
                 (overlay-get ov 'display))
        (setq count (1+ count))))
    count))

(defun test-skg-readable-ids--after-string-count-at-id (id)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward id nil t)
        (dolist (ov (overlays-in (match-beginning 0) (match-end 0)))
          (when (and (overlay-get ov 'skg-magit-title)
                     (overlay-get ov 'after-string))
            (setq count (1+ count)))))
      count)))

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

(ert-deftest test-skg-readable-ids-shortens-inactive-ids-without-title-overlay ()
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
        (insert
         (format
          "* (skg (inactiveNode (id %s) (source private)))\n* (skg (node (id %s) (source public))) active\n"
          test-skg-readable-ids--id-inactive
          test-skg-readable-ids--id-active))
        (setq skg-readable-ids--generation 0)
        (skg-readable-ids--annotate-buffer)
        (should (= 2 (test-skg-readable-ids--display-count)))
        (skg-lp--dispatch-by-type
         nil
         (test-skg-readable-ids--titles-payload
          test-skg-readable-ids--id-active
          "Active Title"))
        (should (= 1 (test-skg-readable-ids--after-string-count)))
        (should (= 0 (test-skg-readable-ids--after-string-count-at-id
                      test-skg-readable-ids--id-inactive)))
        (should (= 1 (test-skg-readable-ids--after-string-count-at-id
                      test-skg-readable-ids--id-active)))))))

(defconst test-skg-readable-ids--id-c
  "55555555-5555-4555-8555-555555555555")

(defconst test-skg-readable-ids--id-d
  "66666666-6666-4666-8666-666666666666")

(defmacro test-skg-readable-ids--with-clean-state (&rest body)
  "Run BODY with the LP machine, pending queue and title cache
rebound to fresh values, the network stubbed out, and every
process-send-string recorded into the local variable `sent'."
  `(let ((skg-response-handler-map nil)
         (skg-lp--pending-count 0)
         (skg-lp--buf (unibyte-string))
         (skg-lp--bytes-left nil)
         (skg-readable-ids--pending-title-requests nil)
         (skg-readable-ids--title-cache
          (make-hash-table :test 'equal))
         (sent nil))
     (ignore sent)
     (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                (lambda () 'fake-proc))
               ((symbol-function 'process-send-string)
                (lambda (_proc string) (push string sent))))
       ,@body)))

(ert-deftest test-skg-readable-ids-cached-titles-need-no-request ()
  "With every ID already cached, annotation is immediate and no
titles-by-ids request is sent -- the point of the cache: a magit
refresh after a staging event costs no server round-trip."
  (test-skg-readable-ids--with-clean-state
   (with-temp-buffer
     (puthash test-skg-readable-ids--id-c "Cached Title"
              skg-readable-ids--title-cache)
     (insert test-skg-readable-ids--id-c)
     (skg-readable-ids--annotate-buffer)
     (should (null sent))
     (should (= 1 (test-skg-readable-ids--after-string-count))))))

(ert-deftest test-skg-readable-ids-requests-only-uncached ()
  "Only uncached IDs are requested; cached ones annotate at once,
and the response's titles merge into the cache."
  (test-skg-readable-ids--with-clean-state
   (with-temp-buffer
     (puthash test-skg-readable-ids--id-c "Cached Title"
              skg-readable-ids--title-cache)
     (insert test-skg-readable-ids--id-c "\n"
             test-skg-readable-ids--id-d "\n")
     (skg-readable-ids--annotate-buffer)
     (should (= 1 (length sent)))
     (should (string-match-p test-skg-readable-ids--id-d (car sent)))
     (should-not (string-match-p test-skg-readable-ids--id-c
                                 (car sent)))
     (should (= 1 (test-skg-readable-ids--after-string-count)))
     (skg-lp--dispatch-by-type
      nil
      (test-skg-readable-ids--titles-payload
       test-skg-readable-ids--id-d "Fetched Title"))
     (should (= 2 (test-skg-readable-ids--after-string-count)))
     (should (equal "Fetched Title"
                    (gethash test-skg-readable-ids--id-d
                             skg-readable-ids--title-cache))))))

(ert-deftest test-skg-readable-ids-missing-cached-and-refreshable ()
  "An ID the server does not answer for is cached as missing, so a
re-annotation sends nothing; `skg-readable-ids-refresh-titles'
clears the cache and asks again."
  (test-skg-readable-ids--with-clean-state
   (with-temp-buffer
     (insert test-skg-readable-ids--id-c)
     (skg-readable-ids--annotate-buffer)
     (should (= 1 (length sent)))
     (skg-lp--dispatch-by-type
      nil
      "((response-type titles-by-ids) (content ()))")
     (should (eq :missing
                 (gethash test-skg-readable-ids--id-c
                          skg-readable-ids--title-cache)))
     (skg-readable-ids--annotate-buffer) ;; like a magit refresh
     (should (= 1 (length sent)))
     (should (= 0 (test-skg-readable-ids--after-string-count)))
     (skg-readable-ids-refresh-titles)
     (should (= 2 (length sent))))))
