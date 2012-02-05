(in-package :mbedit)

(defgeneric json-to-object (json)
  (:documentation "Make a genuine MB object out of the json one."))

(defun mb-json-call (url parameters)
  (let ((drakma:*text-content-types*
         (cons (cons "application" "json") drakma:*text-content-types*))
        (drakma:*drakma-default-external-format* :utf-8))
    (drakma:http-request url :method :get :parameters parameters)))

(defclass json-mb-object ()
  ((comment :reader comment :initarg :comment)
   (name :reader name :initarg :name)
   (id :reader id :initarg :id)
   (gid :reader gid :initarg :gid)))

(defun symbol-to-json-name (sym)
  "Make the JSON name for the given symbol (it will be the key for the hash
table)."
  (substitute #\_ #\- (string-downcase (symbol-name sym))))

(defun read-json-initargs (json-hash expected)
  (unless (= (hash-table-count json-hash)
             (length expected))
    (error "Unexpected length of json object."))
  (mapcan (lambda (sym)
            (multiple-value-bind (hit present?)
                (gethash (symbol-to-json-name sym) json-hash "")
              (unless present?
                (error "Missing slot ~A in json output." sym))
              (list (intern (symbol-name sym) 'keyword) hit)))
          expected))

(defun read-json-object (class other-params json-hash)
  (apply #'make-instance class
         (read-json-initargs json-hash
                             (append other-params '(comment name id gid)))))

(defmacro def-json-class (name &rest slot-names)
  "Define a subclass of JSON-MB-OBJECT with name JSON-<NAME> and slots given by
SLOT-NAMES. These are either symbols or (for cases where we'd clash with :cl or
something) can be a list with two elements. The first is the name of the slot
and the second is the name of the reader.

NAME should be the name of a subclass of MB-OBJECT. The macro also defines a
printer, a reader and a search function and also a method on json-to-object that
produces an MB-OBJECT from a json-object."
  (let* ((slot-forms
          (mapcar (lambda (x)
                    (let* ((slot (if (symbolp x) x (car x)))
                           (reader (if (symbolp x) x (second x)))
                           (initarg (intern (symbol-name slot) 'keyword)))
                      `(,slot :reader ,reader :initarg ,initarg)))
                  slot-names))
         (full-namestring
          (concatenate 'string "JSON-" (symbol-name name)))
         (full-name
          (intern full-namestring :mbedit))
         (reader-name
          (intern (concatenate 'string "READ-" full-namestring)))
         (search-name
          (intern (concatenate 'string full-namestring "-SEARCH")))
         (search-url
          (concatenate 'string "http://musicbrainz.org/ws/js/"
                       (string-downcase (symbol-name name)))))
    `(progn
       (defclass ,full-name (json-mb-object) ,slot-forms)
       (defmethod print-object ((x ,full-name) stream)
         (print-unreadable-object (x stream :type t)
           (format stream "'~A'" (name x))))
       (defun ,reader-name (json-hash)
         (read-json-object ',full-name ',(mapcar #'car slot-forms) json-hash))
       (defun ,search-name (name &optional direct)
         (mapcar #',reader-name
                 (butlast
                  (json:parse
                   (mb-json-call
                    ,search-url
                    (list (cons "q" name)
                          (cons "page" "1")
                          (cons "direct" (if direct "true" "false"))))))))
       (defmethod json-to-object ((x ,full-name))
         (make-instance ',name :id (gid x))))))

(def-json-class artist
    sortname)

(def-json-class recording
    appears-on artist (length recording-length) isrcs)
