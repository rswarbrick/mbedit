(in-package :mbedit)

;; In this code we download "editing" pages to check the JS IDs of the various
;; relationships types.
(defun make-mb-ob (type id)
  (url-to-object (format nil "http://musicbrainz.org/~A/~A" type id)))

(defparameter *rel-type-objects*
  `((artist
     ,(make-mb-ob "artist" "7e84f845-ac16-41fe-9ff8-df12eb32af55")
     ,(make-mb-ob "artist" "fa25cd1f-beeb-4718-b4bb-d3da4f53539f"))
    (release
     ,(make-mb-ob "release" "1735e086-462e-42c3-b615-eebbd5e9f352")
     ,(make-mb-ob "release" "b0c163a5-1165-49c1-8f78-034078eb4556"))
    (recording
     ,(make-mb-ob "recording" "9a8168c1-7475-42ea-89e5-ec306b660b6c")
     ,(make-mb-ob "recording" "501a10df-336f-418c-bdd4-757d5c066cf0"))
    (work
     ,(make-mb-ob "work" "aa1c1ce0-5d39-4e59-bf38-3b9b32347b6f")
     ,(make-mb-ob "work" "ddb3e4c4-bc8f-357d-bb2c-1a88b6ad5516"))))

(defparameter *rel-types-html-strings*
  '((("instrument" artist recording)
     "       performed {additional} {guest} {solo} {instrument} on")
    (("performance" recording work)
     "    is a {partial} {live} {instrumental} {cover} performance of"))
  "This records the html strings used on the edit page for the various
relationship types.")

(defparameter *rel-types-ids* nil
  "This is an alist dictionary, keyed by the triple: relationship type (string),
  class0, class1.")

(defparameter *rel-types-class-pairs-fetched* nil
  "A list of the pairs of classes that have been fetched so far.")
(defparameter *rel-types-class-pairs-stored* nil
  "A list of the pairs of classes that have been fetched so far.")

(defparameter *instrument-tree* nil
  "A list of pairs with instrument name and number.")

(defun relate-edit-page-url (class0 class1)
  (let ((type0 (string-downcase (symbol-name class0)))
        (type1 (string-downcase (symbol-name class1)))
        (obs0 (assoc class0 *rel-type-objects*))
        (obs1 (assoc class1 *rel-type-objects*)))
    (unless (and obs0 obs1)
      (error "didn't find one of the classes: ~a, ~a" class0 class1))
    (format nil "http://musicbrainz.org/edit/relationship/create?~
                 type0=~a&type1=~a&entity0=~a&entity1=~a"
            type0 type1
            (id (second obs0))
            (id (if (eq class0 class1) (third obs1) (second obs1))))))

(defun get-relate-edit-page (class0 class1)
  (ensure-logged-in)
  (drakma:http-request (relate-edit-page-url class0 class1)
                       :cookie-jar *cookie-jar*))

(defun get-select-children (id parsed-data)
  (let ((node (car (stp:filter-recursively
                    (lambda (node)
                      (and (typep node 'stp:element)
                           (string= (stp:local-name node) "select")
                           (awhen (stp:find-attribute-named node "id")
                             (string= (stp:string-value it) id))))
                    parsed-data))))
    (unless node
      (error "Couldn't get the select node called ~A in the given page." id))
    (delete-nils
     (stp:map-children 'list
                       (lambda (option)
                         (awhen (stp:find-attribute-named option "value")
                           (list (stp:data (stp:first-child option))
                                 (stp:value it))))
                       node))))

(defun read-instrument-tree (parsed-data)
  (setf *instrument-tree*
        (mapcar (lambda (pr)
                  (list (string-left-trim '(#\no-break_space) (first pr))
                        (parse-integer (second pr))))
                (get-select-children "id-ar.attrs.instrument.0" parsed-data))))

(defun fetch-relationship-type-texts (class0 class1)
  (let* ((parsed (chtml:parse
                  (get-relate-edit-page class0 class1) (stp:make-builder))))
    (let ((pairs
           (mapcar (lambda (pr)
                     (list (first pr) (parse-integer (second pr))))
                   (get-select-children "id-ar.link_type_id" parsed))))
      (pushnew (cons (list class0 class1) pairs)
               *rel-types-class-pairs-fetched*
               :test #'equalp :key #'car)

      ;; If this happens to have the instruments as well, let's grab them as
      ;; they go by...
      (when (and (eq class0 'artist) (eq class1 'recording))
        (read-instrument-tree parsed))
      pairs)))

(defun get-relationship-type-texts (class0 class1)
  (or (cdr (find (list class0 class1) *rel-types-class-pairs-fetched*
                 :test #'equalp :key #'car))
      (fetch-relationship-type-texts class0 class1)))

(defun store-relationship-type-ids (class0 class1)
  "Read the HTML texts we've pulled and store the actual type ids that we
require for the given pair of classes."
  (let ((pairs (get-relationship-type-texts class0 class1))
        (applicable-strings
         (map-filter (lambda (pair)
                          (destructuring-bind ((type c0 c1) string) pair
                            (when (and (eq class0 c0) (eq class1 c1))
                              (cons string type))))
                     *rel-types-html-strings*))
        (acc nil))
    (dolist (pair applicable-strings)
      (destructuring-bind (text . type) pair
        (let ((hit (find text pairs :key #'car :test #'string=)))
          (unless hit
            (error "can't find the key ~s in html edit page for classes ~a, ~a"
                   text class0 class1))
          (push (list type (second hit)) acc)
          :key #'car :test #'equalp)))
    (setf *rel-types-ids*
          (cons (cons (list class0 class1) acc)
                (remove (list class0 class1) *rel-types-ids*
                        :key #'car :test #'equalp)))
    (values)))

(defun classes-backwards-p (class0 class1)
  (let ((pos0 (position class0 *rel-type-objects* :key #'car))
        (pos1 (position class1 *rel-type-objects* :key #'car)))
    (unless (and pos0 pos1)
      (error "Invalid classes ~A, ~A" class0 class1))
    (< pos1 pos0)))

(defun get-relationship-type-id (type class0 class1)
  "Search for the type id for the given relationship type between the given
classes."
  (when (classes-backwards-p class0 class1)
    (let ((tmp class0))
      (setf class0 class1 class1 tmp)))

  (let ((assoc-hit (assoc (list class0 class1) *rel-types-ids*
                          :test #'equalp)))
    (acond
      ((not assoc-hit)
       (store-relationship-type-ids class0 class1)
       (get-relationship-type-id type class0 class1))
      ((assoc type (cdr assoc-hit) :test #'string=)
       (second it))
      (t
       (error "Could not find type ID for given relationship.")))))

(defun get-instrument-id (instrument-name)
  "Get the ID for a given instrument name."
  (unless *instrument-tree*
    ;; This gets the relevant page
    (fetch-relationship-type-texts 'artist 'recording))
  (second (assoc instrument-name *instrument-tree* :test #'string=)))

(defparameter *relationship-attribute-makers* (make-hash-table :test #'equal))

(defmacro def-relationship-attributes (type class0 class1 &body body)
  "Register something that produces any required extra parameters for a given
type of relationship. BODY is run with RELATION and ATTRIBUTES bound to the
relation and the list of its attribute list, respectively."
  `(setf (gethash ',(list type class0 class1) *relationship-attribute-makers*)
         (lambda (relation)
           (let ((attributes (and (attributes relation)
                                  (attributes (attributes relation)))))
             (declare (ignorable attributes))
             ,@body))))

(def-relationship-attributes "instrument" artist recording
  (list (cons "ar.attrs.instrument.0"
              (princ-to-string (get-instrument-id (first attributes))))))

(def-relationship-attributes "performance" recording work
  nil)

(defun get-relationship-attributes (owner relation)
  "Find any extra parameters that need to be passed to relationship edits for
this relation."
  (let ((class0 (class-name (class-of owner)))
        (class1 (class-name (class-of (target relation)))))
    (when (classes-backwards-p class0 class1)
      (let ((tmp class0)) (setf class0 class1 class1 tmp)))
    (let ((rel-at-maker
           (gethash (list (relation-type relation) class0 class1)
                    *relationship-attribute-makers*)))
      (unless rel-at-maker
        (error "Couldn't find a relationship attribute maker for triple ~A."
               (list (relation-type relation) class0 class1)))
      (funcall rel-at-maker relation))))
