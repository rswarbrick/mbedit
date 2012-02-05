(in-package :mbedit)

(defclass rowed-relation (relation)
  ((row-id :reader row-id :initarg :row-id)))

;; Yes, this is horribly hacky, but I'm not sure what to do better. :-(
(defun make-rowed-relation (relation id)
  (let* ((ob (make-instance 'rowed-relation :row-id id))
         (slot-names
          '("TYPE" "DIRECTION" "ATTRIBUTES" "BEGIN"
            "END" "TARGET-ID" "TARGET"))
         (slots (mapcar (lambda (name) (intern name :mbcl)) slot-names)))
    (dolist (slot slots)
      (when (slot-boundp relation slot)
        (setf (slot-value ob slot) (slot-value relation slot))))
    ob))

(defun recording-dateable-relations (recording)
  (append (relations-of-type recording :class 'work)
          (relations-of-type recording :class 'artist)))

(defun recording-rowed-relations (recording)
  (extract-relation-rows (recording-dateable-relations recording)
                         (drakma:http-request (page recording)
                                              :cookie-jar *cookie-jar*)))

(defun extract-relation-rows (relations html)
  (let ((acc))
    (dolist (rel relations)
      (let ((pos (search (id (target rel)) html)))
        (unless pos
          (error "Can't find a reference to ~A in html page." (target rel)))
        (setf pos (search "http://musicbrainz.org/edit/relationship/delete"
                          html :start2 pos))
        (unless pos
          (error "Can't find an edit link in html page. Am I still logged in?"))
        (multiple-value-bind (hit matches)
            (ppcre:scan-to-strings "=([0-9]+)\">Remove" html :start pos)
          (unless hit
            (error "Failed to find an ID after the edit link."))
          (push (make-rowed-relation rel (parse-integer (aref matches 0)))
                acc))))
    acc))

(defun recording-periods (recording)
  "Return a list of the periods associated to the making of this recording."
  (map-filter (lambda (rel)
                (let ((begin (begin rel))
                      (end (end rel)))
                  (when (or begin end)
                    (list (and begin (parse-date-string begin))
                          (and end (parse-date-string end))))))
              (recording-dateable-relations recording)))

(defun periods-agree-p (periods)
  "Returns true if the periods are exactly identical to one another."
  (every (lambda (x)
           (date= (first x) (first (first periods)))
           (date= (second x) (second (first periods))))
         (cdr periods)))

(defun date-relations (recording begin end &key force auto-edit edit-note)
  "Set the dateable relations on RECORDING to the given dates. If FORCE, ignore
conflicting dates."
  (unless (and (typep begin 'date) (typep end 'date))
    (error "BEGIN and END must be MBCL:DATEs."))
  (let ((relations (recording-rowed-relations recording)))
    (unless (or force
                (periods-agree-p (cons (list begin end)
                                       (recording-periods recording))))
      (error "Recording periods don't all agree for this recording."))

    (dolist (rel relations)
      (unless (and (begin rel) (date= (parse-date-string (begin rel)) begin))
        (actually-date-relation recording rel begin end auto-edit edit-note)))))

(defun date-relation-parameters (recording relation begin end
                                 auto-edit edit-note)
  (multiple-value-bind (source target)
      (get-relation-entities recording relation)
    (let ((class0 (class-name (class-of source)))
          (class1 (class-name (class-of target))))
      `(("ar.entity0.id" . ,(princ-to-string (get-db-row source)))
        ("ar.entity0.name" . ,(moniker source))
        ("ar.entity1.id" . ,(princ-to-string (get-db-row target)))
        ("ar.entity1.name" . ,(moniker target))
        ,@(ar-period-parameters begin end)
        ("ar.link_type_id" . ,(princ-to-string
                               (get-relationship-type-id
                                (relation-type relation) class0 class1)))
        ("ar.edit_note" . ,(or edit-note ""))
        ("ar.as_auto_editor" . ,(if auto-edit "1" "0"))
        ;; These were passed as GET, but hopefully I can be cheeky...
        ("id" . ,(princ-to-string (row-id relation)))
        ("type0" . ,(string-downcase class0))
        ("type1" . ,(string-downcase class1))
        ,@(get-relationship-attributes recording relation)))))

(defun actually-date-relation (recording relation begin end
                               auto-edit edit-note)
  "Actually set the relationship's date."
  (expect-302
    (drakma:http-request
     (format nil "~Aedit/relationship/edit" *mb-root-url*)
     :method :post
     :parameters (date-relation-parameters recording relation
                                           begin end auto-edit edit-note)
     :cookie-jar *cookie-jar*))
  (forget-cached recording)
  (forget-cached (target relation)))

(defun date-some-recordings (recordings begin end &key force auto-edit edit-note)
  "Takes a list of recordings (consider using GET-RELEASE-RECORDINGS) and dates
and tags them. Checks for incompatible dates unless FORCE is given."
  (dolist (rec recordings)
    (format t "Recording '~A'... " (title rec))
    (force-output)
    (date-relations rec begin end
                    :force force :auto-edit auto-edit :edit-note edit-note)
    (format t "done.~%")
    (force-output)))

;; Example usage:
;;
;; (date-some-recordings
;;  (get-release-recordings
;;   (url-to-object "http://musicbrainz.org/release/8c9949c6-6674-40df-b63d-7e6034599819")
;;   '(1 9 1))
;;  (parse-date-string "2008-06")
;;  (parse-date-string "2008-06")
;;  :force t
;;  :edit-note "See edit 16151803")
;;
;; And, now I know it works, I should probably also set auto-edit.
