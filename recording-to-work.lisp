(in-package :mbedit)

(defparameter *performance-link-type-id* 278)

(defun no-works-p (recording)
  "Returns T if a recording is not related to any works."
  (not (relations-of-type recording :class 'work)))

(defun recording-to-work-url (recording work)
  (format nil "~Aedit/relationship/create?type0=recording&type1=work~
               &entity0=~A&entity1=~A"
          *mb-root-url* (id recording) (id work)))

(defun create-relation-parameters (type-id &key begin-date end-date edit-note auto-edit)
  `(("ar.link_type_id" . ,(princ-to-string type-id))
    ,@(ar-period-parameters begin-date end-date)
    ("ar.edit_note" . ,(or edit-note ""))
    ("ar.as_auto_editor" . ,(if auto-edit "1" "0"))))

(defun recording-to-work-parameters (begin-date end-date edit-note auto-edit)
  (create-relation-parameters
   *performance-link-type-id* :begin-date begin-date :end-date end-date
   :edit-note edit-note :auto-edit auto-edit))

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

(defun decide-about-recording (rec work)
  (format t "Recording: ~A~%Title:     ~A~%~A~%~%"
          rec (title rec) (page rec))
  (let ((recognised-names (make-hash-table)))
    (loop
       (let ((decision))
         (when (gethash (title rec) recognised-names)
           (setf decision 1))
         (unless decision
           (format t "Relate?~% (1) Yes~% (2) No~% ~
                                (3) View in browser~% ~
                                (9) Yes, and all with the same name~% ~
                                (*) No and quit~%   -> ")
           (finish-output)
           (setf decision (read)))
         (cond
           ((member decision '(1 9))
            (do-skippable "recording"
              (format t "Relating....")
              (finish-output)
              (relate-recording-to-work rec work :auto-edit t)
              (format t " done.~%")
              (finish-output)
              (when (eql decision 9)
                (setf (gethash (title rec) recognised-names) t)))
            (return t))
           ((eql decision 2)
            (format t "Skipping.~%")
            (return t))
           ((eql decision 3)
            (browse-object rec))
           (t
            (format t "Ok, we're done!~%")
            (return nil)))))))

(defun relate-recordings-to-work (recordings work &optional force)
  "Returns new set of recordings (the ones that still need looking at)."
  (unless (typep work 'work)
    (error "WORK must be a work."))
  (format t "Relating to work: ~A~%Title:            ~A~%~%"
          work (title work))
  (dolist (rec recordings)
    (cond
      (force
       (format t "Relating ~A..." rec)
       (finish-output)
       (relate-recording-to-work rec work :auto-edit t)
       (format t " done.~%"))
      (t
       (unless (decide-about-recording rec work)
         (return))))
    ;; Throw away the head of the queue, since we don't want to be asked about
    ;; it again.
    (pop recordings))
  (format t "~A recordings left in the list." (length recordings))
  recordings)

(defun recordings-for-artist (artist-name search-string)
  (pl-as-list (search-request (format nil "artist:\"~A\" AND ~A"
                                      artist-name search-string)
                              :type "recording")))

(defun recordings-with-no-work (artist-name search-string)
  (remove-if-not #'no-works-p (recordings-for-artist artist-name search-string)))

(defun make-recording-search-string (composer terms)
  (format nil "artist:\"~A\"~{ AND \"~A\"~}"
          (last-word (name composer)) terms))

(defun recordings-with-no-work-search (composer &rest terms)
  "Find recordings with COMPOSER as artist and the given terms in the title (TERMS should be a list of strings). Filters these to just the recordings without a work relationship."
  (remove-if-not #'no-works-p
                 (pl-as-list
                  (search-request (make-recording-search-string composer terms)))))
