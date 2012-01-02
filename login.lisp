(in-package :mbedit)

(defvar *mb-root-url* "http://musicbrainz.org/")
(defvar *mb-login-url* (concatenate 'string *mb-root-url* "login"))
(defvar *mb-session-cookie-name* "musicbrainz_server_session")

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defun extract-login-error (str)
  (let ((start-pos (search "<span class=\"error\"><strong>" str)))
    (when start-pos
      (let ((end-pos (search "</strong>" str :start2 start-pos)))
        (when end-pos
          (subseq str (+ 28 start-pos) end-pos))))))

(define-condition not-302-error (error)
  ((contents :reader contents :initarg :contents)
   (status :reader status :initarg :status)
   (msg :reader msg :initarg :msg :initform nil))
  (:report
   (lambda (c s)
     (format s "Status ~A when we expected 302.~@[ Error message: ~A~]~%"
             (status c) (msg c)))))

(defmacro expect-302 (&body body)
  "Call BODY, which is assumed to end with a call to drakma:http-request. This
is wrapped up in a multiple-value-bind and we check that the exit status is
302. If not, we throw an error (since presumably something went horribly
wrong). If we get data out, the HTML seems to contain the error message in a
reasonably standard format, so try to report that too."
  `(let ((drakma:*drakma-default-external-format* :utf-8))
     (multiple-value-bind (contents status headers) (progn ,@body)
       (unless (= status 302)
         (error 'not-302-error
                :msg (extract-login-error contents)
                :status status :contents contents))
       (cdr (assoc :location headers)))))

(defun login ()
  (handler-case
      (expect-302
        (drakma:http-request *mb-login-url*
                             :method :post
                             :parameters (list (cons "username" *mb-username*)
                                               (cons "password" *mb-password*))
                             :cookie-jar *cookie-jar*))
    (not-302-error (c)
      ;; Set the cookies to nil: you always get a *mb-session-cookie-name*
      ;; cookie, even if you're not logged in, so get rid of that to avoid a
      ;; false positive on ensure-logged-in.
      (setf (drakma:cookie-jar-cookies *cookie-jar*) nil)
      (error c)))
  (values))

(defun ensure-logged-in ()
  (unless (find *mb-session-cookie-name*
                (drakma:cookie-jar-cookies *cookie-jar*)
                :test #'string=
                :key #'drakma:cookie-name)
    (login))
  (values))


