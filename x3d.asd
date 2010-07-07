(asdf:defsystem :x3d
  :components
  ((:file "packages")
   (:module #:src
            :depends-on ("packages")
            :components
            ((:file "x3d")))))
