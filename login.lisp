(in-package :mbedit)

(defvar *mb-login-url* "http://musicbrainz.org/login")
(defvar *mb-session-cookie-name* "musicbrainz_server_session")

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defun extract-login-error (str)
  (let ((start-pos (search "<span class=\"error\"><strong>" str)))
    (when start-pos
      (let ((end-pos (search "</strong>" str :start2 start-pos)))
        (when end-pos
          (subseq str (+ 28 start-pos) end-pos))))))

(defun login ()
  (multiple-value-bind (contents status)
      (drakma:http-request *mb-login-url*
                           :method :post
                           :parameters (list (cons "username" *mb-username*)
                                             (cons "password" *mb-password*))
                           :cookie-jar *cookie-jar*)
    (unless (= status 302)
      ;; Set the cookies to nil: you always get a *mb-session-cookie-name*
      ;; cookie, even if you're not logged in, so get rid of that to avoid a
      ;; false positive on ensure-logged-in.
      (setf (drakma:cookie-jar-cookies *cookie-jar*) nil)
      ;; Now complain.
      (error "Could not log in.~@[ Error message: ~A.~] Status ~A."
             (extract-login-error contents) status))
    (values)))

(defun ensure-logged-in ()
  (unless (find *mb-session-cookie-name*
                (drakma:cookie-jar-cookies *cookie-jar*)
                :test #'string=
                :key #'drakma:cookie-name)
    (login))
  (values))


