;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "../../elisp" default-directory))

(require 'skg-length-prefix)

(ert-deftest test-skg-lp-binary-chunk-with-nonascii-payload-dispatches ()
  "A binary process chunk is already bytes; encoding it again corrupts UTF-8."
  (let* ((skg-lp--buf (unibyte-string))
         (skg-lp--bytes-left nil)
         (skg-response-handler-map nil)
         (payload
          "((response-type titles-by-ids) (content ((id . \"Montoya ñó\"))))")
         (payload-bytes
          (encode-coding-string payload 'utf-8))
         (message-bytes
          (concat
           (format "Content-Length: %d\r\n\r\n"
                   (length payload-bytes))
           payload-bytes))
         (seen nil))
    (skg-register-response-handler
     'titles-by-ids
     (lambda (_tcp-proc payload)
       (setq seen payload))
     nil)
    (skg-lp-handle-generic-chunk nil (substring message-bytes 0 20))
    (skg-lp-handle-generic-chunk nil (substring message-bytes 20))
    (should (equal seen payload))))
