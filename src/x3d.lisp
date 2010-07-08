(in-package :x3d.types)

#+ () (defun sf-bool-p (input)
  "True if INPUT contains true or false as a string."
  (when (stringp input)
    (member input '("True" "False" "") :test #'equalp)))


;;; field types
(defstruct x3d-array-field
  "Abstract field for all types with multiple values.")

(defstruct x3d-field
  "Abstract field for all types with single values.")

(deftype sf-bool ()
  "Single boolean value.

Uninitialized value is FALSE."
  'boolean)

(deftype empty ()
  "Empty list or vector based on the x3d specification.

The spec only refers to a list, but we treat vectors as lists too. Our
  empty vector has no elements in it, but may be of any type."
  '(or null (vector * 0)))

(deftype mf-bool ()
  "Multiple boolean values.

Uninitialized value is `empty'."
  '(vector sf-bool))

(deftype sf-double ()
  "Double precision floating point number.

Uninitialized value is 0.0"
  'double-float)

(deftype mf-double ()
  "Multiple doubles.

Uninitialized value is `empty'."
  '(vector 'sf-double))

(deftype sf-float ()
  "Single precision float.

Uninitialized value is 0.0."
  'single-float)

(deftype mf-float ()
  "Multiple single floats

Uninitialized value is `empty'."
  '(vector sf-float))

;;; TODO: may want to make this a struct instead of an array, but we can
;;; do this later when we have a better idea of requirements
(deftype sf-color ()
  "One RGB (read green blue) color triple.

Uninitialized value is (0 0 0)."
  '(vector (float 0.0 1.0) 3))

(deftype mf-color ()
  "Multiple colors.

Uninitialized value is `empty'."
  '(vector (vector sf-color)))

(in-package :x3d.parse)

(defun parse-sf-bool (string)
  (declare (string string))
  (check-type string (satisfies sf-bool-p))
  (equalp "True" string))


(defun parse-points (string)
  "Parse a list of .x3d points into a list of vectors.

Our result is something like (list #(1 2 3) #(3.4 3.4 4.5))"
  (declare (string string))
  (mapcar (lambda (vect)
            (mapcar #'read-from-string
                    (split-sequence:split-sequence
                     #\Space vect :remove-empty-subseqs t)))
          (remove " "
                  (split-sequence:split-sequence #\, string :remove-empty-subseqs t)
                  :test #'string=)))
