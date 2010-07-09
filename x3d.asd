(asdf:defsystem :x3d
  :depends-on (:cxml :cxml-dom)
  :components
  ((:file "packages")
   (:module #:src
            :depends-on ("packages")
            :components
            ((:file "x3d")))))
