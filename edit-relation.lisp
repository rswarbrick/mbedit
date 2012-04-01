(in-package :mbedit)

;; Irritatingly, editing relationships requires their row ids, which you can't
;; get from the XML service. Grr. The solution seems to be to extract "rowed
;; relations" by scraping the HTML. For now, this is only going to work with
;; relationships that have a recording at one end.

(defvar *relationship-row-cache* (make-hash-table :test #'equalp)
  "This is keyed by the triple (source-id relation-type target-id), since MB
only allows one relationship for each such triple. The data stored is the row
id.")

(defmethod relation-key ((source mb-object) (relation relation))
  (multiple-value-bind (src tgt) (get-relation-entities source relation)
    (list (id src) (relation-type relation) (id tgt))))

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

(defun extract-recording-relation-rows (relations html)
  "Reads the HTML of a recording page and returns a list of pairs (RELN ROW-ID),
one for each relation in RELATIONS."
  (let ((acc)
        (relationships-header-pos (search "<h2>Relationships</h2>" html)))
    ;; You have to jump past here first, otherwise you pick up release artist
    ;; credits and everything goes pear shaped...
    (unless relationships-header-pos
      (error "Couldn't find relationships header."))
    (dolist (rel relations)
      (let ((pos (search (id (target rel)) html :start2 relationships-header-pos)))
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
          (push (list rel (parse-integer (aref matches 0))) acc))))
    acc))

(defun store-relation-row-id (source relation id)
  (setf (gethash (relation-key source relation) *relationship-row-cache*) id))

(defun snarf-recording-relation-rows (recording)
  (map nil
       (lambda (pair)
         (store-relation-row-id recording (first pair) (second pair)))
       (extract-recording-relation-rows
        (relations-of-type recording)
        (drakma:http-request (page recording) :cookie-jar *cookie-jar*))))

(defgeneric get-rowed-relation (source relation)
  (:documentation "Given a relation, try to extract a row ID for it and return a
ROWED-RELATION."))

(defmethod get-rowed-relation (source (relation rowed-relation))
  (declare (ignore source))
  relation)

(defmethod get-rowed-relation ((source mb-object) (relation relation))
  (let* ((key (relation-key source relation))
         (hit (gethash key *relationship-row-cache*)))
    (unless hit
      (cond
        ((typep source 'recording)
         (snarf-recording-relation-rows source))
        ((typep (target relation) 'recording)
         (snarf-recording-relation-rows (target relation)))
        (t
         (error "Relation row id not known and neither end is a recording.")))
      (setf hit (gethash key *relationship-row-cache*))
      (unless hit
        (error "Despite updating, cannot find row id for relation.")))
    (make-rowed-relation relation hit)))

(defgeneric edit-relation-parameters (owner relation
                                      &key source target begin end type
                                        edit-note auto-edit)
  (:documentation "Get the parameters to give the web-service call in order to
edit the relationship as desired. Attribute changes aren't currently
supported (although we automatically drop old attributes if the type
changes)."))

(defmethod edit-relation-parameters ((owner mb-object) (relation rowed-relation)
                                     &key source target begin end type
                                       edit-note auto-edit)
  (let ((new-relation
         (make-relation (or type (relation-type relation))
                        (or target (target relation))
                        :direction (direction relation)
                        :attributes (if type nil (attributes relation))
                        :begin (or begin (begin relation))
                        :end (or end (end relation)))))
    (multiple-value-bind (real-src real-tgt)
        (get-relation-entities (or source owner) new-relation)
      (let* ((class0 (class-name (class-of real-src)))
             (class1 (class-name (class-of real-tgt))))
        `(("ar.entity0.id" . ,(princ-to-string (get-db-row real-src)))
          ("ar.entity0.name" . ,(moniker real-src))
          ("ar.entity1.id" . ,(princ-to-string (get-db-row real-tgt)))
          ("ar.entity1.name" . ,(moniker real-tgt))
          ,@(ar-period-parameters (begin new-relation) (end new-relation))
          ("ar.link_type_id" . ,(princ-to-string
                                 (get-relationship-type-id
                                  (relation-type new-relation) class0 class1)))
          ("ar.edit_note" . ,(or edit-note ""))
          ("ar.as_auto_editor" . ,(if auto-edit "1" "0"))
          ;; These were passed as GET by the website, but seem to work fine as
          ;; POST parameters, thank goodness.
          ("id" . ,(princ-to-string (row-id relation)))
          ("type0" . ,(string-downcase class0))
          ("type1" . ,(string-downcase class1))
          ,@(get-relationship-attributes (or source owner) new-relation))))))

(defmethod edit-relation-parameters ((owner mb-object) (relation relation)
                                     &rest args)
  (apply #'edit-relation-parameters
         owner (get-rowed-relation owner relation) args))

(defgeneric edit-relation (owner relation
                           &key source target begin end type
                             edit-note auto-edit)
  (:documentation "Actually edit a relationship."))

(defmethod edit-relation ((owner mb-object) (relation relation)
                          &rest args &key target source &allow-other-keys)
  "Actually set the relationship's date."
  (expect-302
    (drakma:http-request
     (format nil "~Aedit/relationship/edit" *mb-root-url*)
     :method :post
     :parameters (apply #'edit-relation-parameters owner relation args)
     :cookie-jar *cookie-jar*))
  (forget-cached owner)
  (forget-cached (target relation))
  (when target (forget-cached target))
  (when source (forget-cached source))
  (values))
