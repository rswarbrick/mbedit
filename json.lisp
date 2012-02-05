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

(defgeneric really-get-db-row (mb-object)
  (:documentation
   "Using some nasty internal JSON based stuff, get the DB row id for the given
object."))

(defgeneric get-db-row (mb-object)
  (:documentation
   "Find the DB row for a given MB-OBJECT (required for some edits). Caches the
result."))

(defmethod get-db-row ((mbo mb-object))
  (or (gethash (id mbo) *db-row-cache*)
      (setf (gethash (id mbo) *db-row-cache*) (really-get-db-row mbo))))

(defun perform-json-search (search-function mbid moniker)
  "Do the JSON search required for the given object. We first try an indexed
search (since it's more polite), then run direct searches up to 10 pages
deep. The search function has arguments (NAME &key DIRECT PAGE)."
  (flet ((find-match (lst)
           (find-if (lambda (x) (string= (gid x) mbid)) lst)))
    (or (find-match (funcall search-function moniker))
        (dolist (page (alexandria:iota 10 :start 1))
          (let ((results (funcall search-function moniker
                                  :direct t :page page)))
            ;; If we ran out of actual pages, we get NIL so give up.
            (unless results (return nil))
            (awhen (find-match results) (return it)))))))

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
       (defun ,search-name (name &key direct (page 1))
         (mapcar #',reader-name
                 (butlast
                  (json:parse
                   (mb-json-call
                    ,search-url
                    (list (cons "q" name)
                          (cons "page" (princ-to-string page))
                          (cons "direct" (if direct "true" "false"))))))))
       (defmethod json-to-object ((x ,full-name))
         (make-instance ',name :id (gid x)))
       (defmethod really-get-db-row ((x ,name))
         (let ((hit (perform-json-search #',search-name (id x) (moniker x))))
           (unless hit
             (error "Can't find DB row for ~A" x))
           ;; I have no idea why, but small numbers get sent as integers over
           ;; JSON (for example, Chopin is "id":83 and larger ones get sent as
           ;; strings. Ho hum.
           (nth-value 0
                      (if (integerp (id hit))
                          (id hit) (parse-integer (id hit)))))))))

(def-json-class artist sortname)
(def-json-class recording appears-on artist (length recording-length) isrcs)
(def-json-class release)
(def-json-class work artists)
