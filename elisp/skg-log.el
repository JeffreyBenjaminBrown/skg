;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Structured logging for the skg Emacs client.
;;;
;;; Output: JSON lines to <skg-log-file>, one object per entry.
;;; Each entry has: timestamp, level, caller, category, message.
;;;
;;; Toggle: set skg-log-file to nil to disable all logging.
;;; Filter: set skg-log-categories to a list of symbols to restrict
;;;         which categories are logged. Empty list = log everything.
;;; Level:  set skg-log-min-level to 'debug, 'info, 'warn, or 'error.
;;;
;;; USAGE:
;;;   (skg-log 'info 'save "sending %d bytes" content-length)
;;;   (skg-log 'debug 'tcp "received %s" payload)
;;;   (skg-log 'warn 'uri "nil skg-view-uri in %s" (buffer-name))
;;;   (skg-log 'error 'parse "failed: %S" err)

(defvar skg-log-file nil
  "Path to the skg client log file. Nil disables logging.")

(defvar skg-log-categories '()
  "List of enabled log categories (symbols).
Empty list means log everything.")

(defvar skg-log-min-level 'info
  "Minimum log level. One of: debug, info, warn, error.")

(defconst skg-log--level-order
  '((debug . 0) (info . 1) (warn . 2) (error . 3))
  "Numeric ordering for log levels.")

(defun skg-log--level-num (level)
  "Return numeric value for LEVEL."
  (or (alist-get level skg-log--level-order) 1))

(defun skg-log--caller ()
  "Return the name of the function that called skg-log.
Walks the backtrace to find the first non-skg-log frame."
  (let ((n 0)
        (result "?"))
    (while (< n 20)
      (let ((frame (backtrace-frame (setq n (1+ n)))))
        (when frame
          (let ((fn (cadr frame)))
            (when (and (car frame) (symbolp fn))
              (let ((name (symbol-name fn)))
                (unless (or (string-prefix-p "skg-log" name)
                            (string= name "apply")
                            (string= name "funcall"))
                  (setq result name
                        n 20))))))))
    result))

(defun skg-log (level category fmt &rest args)
  "Log a structured JSON entry if LEVEL and CATEGORY pass filters.
LEVEL is a symbol: debug, info, warn, error.
CATEGORY is a symbol: save, tcp, search, uri, etc.
FMT and ARGS are passed to `format'."
  (when (and skg-log-file
             (>= (skg-log--level-num level)
                  (skg-log--level-num skg-log-min-level))
             (or (null skg-log-categories)
                 (memq category skg-log-categories)))
    (let* ((msg (apply #'format fmt args))
           (caller (skg-log--caller))
           (entry (format "{\"ts\":\"%s\",\"level\":\"%s\",\"cat\":\"%s\",\"fn\":\"%s\",\"msg\":%s}\n"
                          (format-time-string "%Y-%m-%dT%H:%M:%S.%3N")
                          level
                          category
                          caller
                          (json-encode msg))))
      (let ((inhibit-message t))
        (write-region entry nil skg-log-file t 'silent)))))

(provide 'skg-log)
