;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:curves
  (:use #:cl #:alexandria #:3d-vectors #:3d-matrices )
  (:export #:hermite-curve
           #:gl-cubic-polynomial
           #:gl-double-helix
           #:random-curves))
