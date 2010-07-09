(defpackage #:x3d.types
  (:use :cl)
  (:export #:empty))

(defpackage #:x3d.parse
  (:use :cl))

(defpackage #:x3d
  (:use :cl))

(in-package :x3d)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter +root-directory+
    (make-pathname :directory
                   (pathname-directory
                    (load-time-value
                     (or #.*compile-file-truename*
                         *load-truename*))))
    "The root of the x3d directory.

We use this for locating data and configuration information for HoW. This
may run into some issues in the future but for the near term future this
solves most issues."))
