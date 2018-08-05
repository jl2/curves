;;;; curves.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:curves
  :description "Describe curves here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:clgl #:3d-vectors #:3d-matrices #:alexandria)
  :components ((:file "package")
               (:file "curves")))
