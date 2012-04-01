(in-package :mbedit)

;; String quartets are not orchestras. Unfortunately, switching "performed
;; orchestra" relationships to "performed" by hand takes ages. A perfect job for
;; mbedit!

(defgeneric orchestral-relationships (object)
  (:documentation "Returns all 'performed orchestra' relationships on OBJECT."))

(defmethod orchestral-relationships ((recording recording))
  (remove-if-not
   (lambda (rel) (string= (relation-type rel) "performing orchestra"))
   (relations-of-type recording :class 'artist)))

(defmethod orchestral-relationships ((release release))
  (mapcan #'orchestral-relationships
          (pl-as-list (recordings release))))

(defgeneric unorchestrate-relationships (object)
  (:documentation "Turns all 'performed orchestra' relationships on OBJECT into
generic 'performer' relationships."))

(defmethod unorchestrate-relationships ((recording recording))
  (let ((printed))
    (dolist (rel (orchestral-relationships recording))
      (unless printed
        (format t "Recording: ~A  "
                (shortened-string (title recording) :max-length 60))
        (force-output)
        (setf printed t))
      (edit-relation recording rel
                     :type "performer"
                     :edit-note "Removing incorrect orchestra type"
                     :auto-edit t)
      (princ #\.) (force-output))
    (when printed
      (format t " done.~%")
      (force-output))
    (values)))

(defmethod unorchestrate-relationships ((release release))
  (map nil #'unorchestrate-relationships (pl-as-list (recordings release))))
