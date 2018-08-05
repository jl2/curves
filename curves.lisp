;;;; curves.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:curves)

(declaim (inline iterate-along))
(defun iterate-along (curve-function t-min t-max steps line-function)
  (declare (type function curve-function line-function))
  (let ((dt (/ (- t-max t-min) steps)))
    (dotimes (i steps)
      (let ((tv1 (+ t-min (* dt i)))
            (tv2 (+ t-min (* dt (1+ i)))))
        (funcall line-function
                 (funcall curve-function tv1)
                 (funcall curve-function tv2))))))

(defun gl-cubic-polynomial (A B C D 
                              &key 
                              (steps 20)
                                (color (vec4 0.0 1.0 0.0 1.0)))
  
  (declare (type vec3 A B C D) 
           (type fixnum steps))
  (let* ((prims (make-instance 'clgl:primitives))
         (Pt D)
         (delta (/ 1.0 steps))
         (v-delta (vec3 delta delta delta))
         (dddp (v* a 6.0 v-delta v-delta v-delta))
         (ddp (v+ (v* 2 b v-delta v-delta) dddp))
         (dp (v+ (v* a v-delta v-delta v-delta)
                 (v* b v-delta v-delta)
                 (v* c v-delta ))))

    (flet ((gl-line (pt1 pt2)
             (clgl:add-line prims pt1 pt2 color)))
      (declare (inline gl-line))

      (dotimes (i steps)
        (let ((np (v+ Pt dp)))
          (gl-line Pt np)
          (setf Pt np)
          (nv+ dp ddp)
          (nv+ ddp dddp))))
    prims))

(defun random-point-near (pt radius)
  (v+ pt (vec3-random (- radius) radius)))

(defun random-curves (viewer &key (x-steps 10)  (y-steps 10))
  (let (( oname 0))
    (dotimes (i x-steps)
      (let ((uv (- (* i (/ 2.0 x-steps)) 1.0)))
        (dotimes (j y-steps)
          (let* ((vv (- (* j (/ 2.0 x-steps)) 1.0))
                 (dz (/ 3.0 4.0))
                 (ddz (/ dz 4.0)))
            (clgl:add-object viewer
                             oname
                             (gl-cubic-polynomial (vec3 uv vv 0.0)
                                                  (random-point-near (vec3 uv vv dz) ddz)
                                                  (random-point-near (vec3 uv vv (* 2 dz)) ddz)
                                                  (random-point-near (vec3 uv vv (* 3 dz)) ddz))))
          (incf oname))))))
