(in-package :mbedit)

(defparameter *relate-part-type-id* 281)
(defparameter *relate-composer-type-id* 168)

(defun get-create-url (type)
  (format nil "~A~A/create" *mb-root-url* type))

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

(defun create-dated-work (composer name begin-date end-date)
  (format t "~A~%  Creating work... " name)
  (finish-output)
  (let ((work (create-work-with-name name)))
    (format t "done.~%  Relating to composer... ")
    (finish-output)
    (relate-to-composer work composer
                        :begin-date begin-date
                        :end-date end-date)
    (format t "done.~%")
    work))

(defun add-works-below-parent (parent composer names &key begin-date end-date)
  (mapcar (lambda (name)
            (let ((work (create-dated-work composer name begin-date end-date)))
              (format t "  Relating to parent... ")
              (finish-output)
              (relate-to-parent work parent)
              (format t "done.~%~%")
              work))
          names))

(defun add-work-list (composer parent other-names
                      &optional begin-date end-date)
  "Add a given work list. If BEGIN-DATE is specified and not END-DATE, makes
them equal. Returns the parent work, which is created if PARENT is a string and
is merely used if it is an MB-OBJECT."
  (unless end-date
    (setf end-date begin-date))
  
  (cond
    ((stringp parent)
     (setf parent (create-dated-work composer parent begin-date end-date)))
    ((typep parent 'work))
    (t
     (error "PARENT must be either a string or an work object (got ~A)."
            parent)))

  (add-works-below-parent parent composer other-names
                          :begin-date begin-date :end-date end-date)
  parent)

(defun make-standard-work-list (composer parent movement-names
                                &key begin-date end-date (first-index 1))
  "Returns the parent work. Like ADD-WORK-LIST, but automatically prepends the
parent name to each movement name, along with a roman numeral."
  (let ((parent-name (if (stringp parent) parent (title parent))))
    (add-work-list
     composer parent
     (mapcar (lambda (n mvmnt) (format nil "~A: ~@R. ~A" parent-name n mvmnt))
             (alexandria:iota (length movement-names) :start first-index)
             movement-names)
     begin-date end-date)))

(defun fformat (control-string &rest format-arguments)
  "Format to standard output and flush."
  (format t control-string format-arguments)
  (force-output)
  (values))

(defun make-opera-or-ballet (composer tree &key begin-date end-date)
  "Make an entire opera/ballet work tree. (CAR TREE) should be the name of the
work, then the rest of the elements should be lists of scenes/numbers."
  (unless end-date
    (setf end-date begin-date))

  (fformat "Creating ~S... " (first tree))
  (let* ((work-name (first tree))
         (acts (rest tree))
         (parent (create-dated-work composer work-name begin-date end-date)))
    (fformat "done.~%")
    (loop for act in acts for act-no from 1
       do
         (progn
           (fformat " Act ~D. Creating act work... " act-no)
           (let ((act-work
                  (create-dated-work
                   composer (format nil "~A: Act ~@R" work-name act-no)
                   begin-date end-date)))
             (fformat "done.~%   Relating to parent... ")
             (relate-to-parent act-work parent)
             (fformat "done.~%")
             (loop for scene in act for num from 1
                do
                  (let ((scene-name
                         (format nil "~A: Act ~@R. ~D. ~A~%"
                                 work-name act-no num scene))
                        scene-work)
                    (fformat "  Creating ~S... " scene-name)
                    (setf scene-work
                          (create-dated-work composer scene-name
                                             begin-date end-date))
                    (fformat "done.~%    Relating to parent... ")
                    (relate-to-parent scene-work act-work)
                    (fformat "done.~%"))))))
    parent))
