(in-package :mbedit)

(defun track-title-recording-pairs (release)
  (mapcar (lambda (trk) (list (title trk) (recording trk)))
          (tracks release)))

(defun tracks-with-recording (recording)
  (mapcan
   (lambda (release)
     (remove-if-not
      (lambda (trk) (string= (id (recording trk)) (id *rec*)))
      (tracks release)))
   (pl-as-list (release-list recording))))

(defun recording-needs-renaming-p (new-name recording)
  (and
   (not (or (null new-name)
            (string= new-name (title recording))))
   (= 1 (length (tracks-with-recording recording)))))

(defun pairs-to-rename (release)
  (remove-if-not (lambda (pair) (apply #'recording-needs-renaming-p pair))
                 (track-title-recording-pairs release)))

(defun get-edit-url (object)
  (format nil "~A~A/~A/edit" *mb-root-url* (table-name object) (id object)))

(defun edit-recording-parameters (new-name recording)
  (let* ((name-credits (name-credits (artist-credit recording)))
         (nc (first name-credits)))
    (unless (and (= 1 (length name-credits)) (null (join-phrase nc)))
      (error "Artist credit too complicated for me!"))

    (list (cons "edit-recording.name" new-name)
          (cons "edit-recording.comment" (or (disambiguation recording) ""))
          (cons "edit-recording.length" (aif (recording-length recording)
                                             (format-time-period it)
                                             ""))
          (cons "edit-recording.edit_note" "")
          (cons "edit-recording.as_auto_editor" "1")

          (cons "edit-recording.artist_credit.names.0.artist.name"
                (name (artist nc)))
          (cons "edit-recording.artist_credit.names.0.name" (name nc))
          (cons "edit-recording.artist_credit.names.0.join_phrase" "")
          ;; I think this can be bogus (looking at
          ;; MusicBrainz/Server/Controller/Recording/Edit.pm)
          (cons "edit-recording.artist_credit.names.0.artist.id" "1"))))

(defun rename-recording (new-name recording)
  (expect-302
    (drakma:http-request
     (get-edit-url recording)
     :method :post
     :parameters (edit-recording-parameters new-name recording)
     :cookie-jar *cookie-jar*))
  (values))

(defun track-names-to-recordings (release)
  (dolist (pair (pairs-to-rename release))
    (destructuring-bind (new-name recording) pair
      (format t "Renaming recording from:~%  ~A~%to~%  ~A~%... "
              (title recording) new-name)
      (finish-output)
      (rename-recording new-name recording)
      (format t "Done.~%")
      (finish-output)
      (forget-cached recording)))
  (forget-cached release)
  (values))
