(in-package :mbedit)

(defparameter *performance-link-type-id* 278)

(defun no-works-p (recording)
  "Returns T if a recording is not related to any works."
  (not (relations-of-type recording :class 'work)))

(defun recording-to-work-url (recording work)
  (format nil "~Aedit/relationship/create?type0=recording&type1=work~
               &entity0=~A&entity1=~A"
          *mb-root-url* (id recording) (id work)))

(defun date-parameters (name date)
  "Return a list of parameters <name>.year, <name>.month and <name>.day filled
in from DATE (which can be NIL)."
  (mapcar (lambda (suffix accessor)
            (cons (concatenate 'string name "." suffix)
                  (aif (and date (funcall accessor date))
                       (princ-to-string it)
                       "")))
          '("year" "month" "day")
          '(year month day)))

(defun recording-to-work-parameters
    (begin-date end-date edit-note auto-edit)
  `(("ar.link_type_id" . ,(princ-to-string *performance-link-type-id*))
    ,@(date-parameters "ar.begin_date" begin-date)
    ,@(date-parameters "ar.end_date" end-date)
    ("ar.edit_note" . ,(or edit-note ""))
    ("ar.as_auto_editor" . ,(if auto-edit "1" "0"))))

(defun relate-recording-to-work
    (recording work &key begin-date end-date edit-note auto-edit)
  (expect-302
    (drakma:http-request
     (recording-to-work-url recording work)
     :method :post
     :parameters (recording-to-work-parameters
                  begin-date end-date edit-note auto-edit)
     :cookie-jar *cookie-jar*))
  (forget-cached recording)
  (forget-cached work))
