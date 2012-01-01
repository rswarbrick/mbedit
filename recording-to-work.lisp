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

(defvar *r-to-w-queue* nil
  "Fill this up before calling RELATE-RECORDINGS-TO-WORK")
(defvar *r-to-w-work* nil
  "Set this before calling RELATE-RECORDINGS-TO-WORK")

(defun decide-about-recording (rec work)
  (format t "Recording: ~A~%~A~%~%" rec (page rec))
  (loop
     (format t "Relate?~% (1) Yes~% (2) No~% ~
                          (3) Inspect~% (*) No and quit~%   -> ")
     (finish-output)
     (let ((decision (read)))
       (cond
         ((eql decision 1)
          (format t "Relating....")
          (finish-output)
          (relate-recording-to-work rec work :auto-edit t)
          (format t " done.~%")
          (return t))
         ((eql decision 2)
          (format t "Skipping.~%")
          (return t))
         ((eql decision 3)
          (inspect rec))
         (t
          (format t "Ok, we're done!~%")
          (return nil))))))

(defun relate-recordings-to-work (&optional force)
  (unless (typep *r-to-w-work* 'work)
    (error "*R-TO-W-WORK* must be a work."))
  (format t "Relating to the work: ~A~%~%" *r-to-w-work*)
  (dolist (rec *r-to-w-queue*)
    (cond
      (force
       (format t "Relating ~A..." rec)
       (finish-output)
       (relate-recording-to-work rec *r-to-w-work* :auto-edit t)
       (format t " done.~%"))
      (t
       (unless (decide-about-recording rec *r-to-w-work*)
         (return))))
    ;; Throw away the head of the queue, since we don't want to be asked about
    ;; it again.
    (pop *r-to-w-queue*))
  (format t "~A recordings left in the list." (length *r-to-w-queue*))
  (values))

(defun recordings-for-artist (artist-name search-string)
  (pl-as-list (search-request (format nil "artist:\"~A\" AND ~A"
                                      artist-name search-string)
                              :type "recording")))

(defun recordings-with-no-work (artist-name search-string)
  (remove-if-not #'no-works-p (works-for-artist artist-name search-string)))
