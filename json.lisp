(in-package :mbedit)

(defgeneric json-to-object (json)
  (:documentation "Make a genuine MB object out of the json one."))

(defun mb-json-call (url parameters)
  (let ((drakma:*text-content-types*
         (cons (cons "application" "json") drakma:*text-content-types*))
        (drakma:*drakma-default-external-format* :utf-8))
    (drakma:http-request url :method :get :parameters parameters)))

(defclass json-artist ()
  ((sortname :reader sortname :initarg :sortname)
   (comment :reader comment :initarg :comment)
   (name :reader name :initarg :name)
   (id :reader id :initarg :id)
   (gid :reader gid :initarg :gid)))

(defmethod print-object ((ja json-artist) stream)
  (print-unreadable-object (ja stream :type t)
    (format stream "'~A'" (name ja))))

(defun read-json-initargs (json-hash expected)
  (unless (= (hash-table-count json-hash)
             (length expected))
    (error "Unexpected length of json object."))
  (mapcan (lambda (sym)
            (let ((sym-name (symbol-name sym)))
              (multiple-value-bind (hit present?)
                  (gethash (string-downcase sym-name) json-hash "")
                (unless present?
                  (error "Missing slot ~A in json output." sym-name))
                (list (intern sym-name 'keyword) hit))))
          expected))

(defun read-json-artist (json-hash)
  (apply #'make-instance 'json-artist
         (read-json-initargs json-hash '(sortname comment name id gid))))

(defun json-artist-search (name &optional direct)
  (mapcar #'read-json-artist
          (butlast
           (json:parse
            (mb-json-call
             "http://musicbrainz.org/ws/js/artist"
             (list (cons "q" name)
                   (cons "page" "1")
                   (cons "direct" (if direct "true" "false"))))))))

(defmethod json-to-object ((ja json-artist))
  (make-instance 'artist :id (gid ja)))
