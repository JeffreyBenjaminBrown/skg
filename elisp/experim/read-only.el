;;; -*- lexical-binding: t; -*-
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
