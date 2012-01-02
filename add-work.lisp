(in-package :mbedit)

(defparameter *relate-part-type-id* 281)
(defparameter *relate-composer-type-id* 168)

(defun get-create-url (type)
  (format nil "~A~A/create" *mb-root-url* type))

(defun parse-mb-object-url (url)
  (destructuring-bind (type id) (last (split-sequence #\/ url) 2)
    (values type id)))

(defun url-to-object (url)
  (multiple-value-bind (type id) (parse-mb-object-url url)
    (let ((sym (find-symbol (string-upcase type) :mbedit)))
      (unless sym (error "Unrecognised table name: ~A" type))
      (make-instance sym :id id))))

(defun create-work-with-name (name &key comment iswc type-id)
  "Returns a work object representing the one we made."
  (url-to-object
   (expect-302
     (drakma:http-request
      (get-create-url "work")
      :method :post
      :parameters (list (cons "edit-work.name" name)
                        (cons "edit-work.comment" (or comment ""))
                        (cons "edit-work.iswc" (or iswc ""))
                        (cons "edit-work.type_id" (or type-id ""))
                        (cons "edit-work.as_auto_editor" "0"))
      :cookie-jar *cookie-jar*))))

(defun create-relation-url (type0 type1 entity0 entity1)
  (format nil "~Aedit/relationship/create?type0=~A&type1=~A~
               &entity0=~A&entity1=~A"
          *mb-root-url* type0 type1 entity0 entity1))

(defun relate-to-parent-url (child parent)
  (create-relation-url "work" "work" (id parent) (id child)))

(defun relate-to-parent-parameters (edit-note auto-edit)
  (create-relation-parameters
   *relate-part-type-id* :edit-note edit-note :auto-edit auto-edit))

(defun relate-to-parent (child parent)
  (expect-302
    (drakma:http-request
     (relate-to-parent-url child parent)
     :method :post
     :parameters (relate-to-parent-parameters nil t)
     :cookie-jar *cookie-jar*))
  (values))

(defun relate-to-composer-url (work composer)
  (create-relation-url "work" "artist" (id work) (id composer)))

(defun relate-to-composer-parameters (begin-date end-date edit-note auto-edit)
  (create-relation-parameters
   *relate-composer-type-id* :begin-date begin-date :end-date end-date
   :edit-note edit-note :auto-edit auto-edit))

(defun relate-to-composer (work composer &key begin-date end-date)
  (expect-302
    (drakma:http-request
     (relate-to-composer-url work composer)
     :method :post
     :parameters (relate-to-composer-parameters begin-date end-date nil t)
     :cookie-jar *cookie-jar*))
  (values))



(defun add-works-below-parent (parent composer names &key begin-date end-date)
  (mapcar (lambda (name)
            (format t "~A~%  Creating work... " name)
            (finish-output)
            (let ((work (create-work-with-name name)))
              (format t "done.~%  Relating to parent... ")
              (finish-output)
              (relate-to-parent work parent)
              (format t "done.~%  Relating to composer... ")
              (finish-output)
              (relate-to-composer work composer
                                  :begin-date begin-date
                                  :end-date end-date)
              (format t "done.~%~%")
              work))
          names))
