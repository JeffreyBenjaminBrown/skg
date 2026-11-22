;;; read-only.el --- Scratch experiment with the read-only text property  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Loaded by nothing; a scratch file of forms to evaluate one at a
;;; time (C-x C-e), trying out the 'read-only' text property on a
;;; region. The positions the forms below name are examples from one
;;; session; evaluate '(point)' to find your own.
;;;
;;; Code:

(point) ;; if point is just after the ), it is at 8
(point) ;; if point is just after the ), it is at 60

;; If any part of the region is already read-only,
;; inhibit-read-only is needed (see below)
;; to enable changing the region's properties.

(let ((inhibit-read-only t))
  (put-text-property 1 8 'read-only
		     nil)) ;; nil -> rw, t -> read only
(let ((inhibit-read-only t))
  (put-text-property 1 45 'read-only
		     nil)) ;; nil -> rw, t -> read only
