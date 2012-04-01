(in-package :mbedit)

(defvar *db-row-cache* (make-hash-table :test #'equalp)
  "Cache for finding DB rows from MBIDs.")
(defvar *tracks-to-recordings-debug* t
  "Print a message if we're skipping a track because it occurs on more than one
release.")

(defun track-recording-pairs (release)
  (mapcar (lambda (trk) (list trk (recording trk))) (tracks release)))

(defun tracks-with-recording (recording)
  (mapcan
   (lambda (release)
     (remove-if-not
      (lambda (trk) (string= (id (recording trk)) (id recording)))
      (tracks release)))
   (pl-as-list (release-list recording))))

(defun artist-credits-agree-p (track recording)
  (or (null (artist-credit track))
      (equalp (artist-credit-string (artist-credit track))
              (artist-credit-string (artist-credit recording)))))

(defun titles-agree-p (track recording)
  (or (null (title track))
      (string= (title track) (title recording))))

(defun recording-needs-renaming-p (track recording &optional force)
  (and
   (not (and (titles-agree-p track recording)
             (artist-credits-agree-p track recording)))
   (if (or force
           (= 1 (length (tracks-with-recording recording))))
       t
       (when *tracks-to-recordings-debug*
         (format t "Not renaming recording '~A' (track name '~A') because ~
                    it appears on multiple releases.~%"
                 (title recording) (title track))
         (force-output)
         nil))))

(defun get-edit-url (object)
  (format nil "~A~A/~A/edit" *mb-root-url* (table-name object) (id object)))

(defun edit-recording-parameters (recording
                                  &key edit-note auto-edit
                                    ac title comment length)
  (let* ((name-credits (name-credits (or ac (artist-credit recording))))
         (nc (first name-credits)))
    (unless (and (= 1 (length name-credits)) (null (join-phrase nc)))
      (error "Artist credit too complicated for me!"))

    (list (cons "edit-recording.name" (or title (title recording) ""))
          (cons "edit-recording.comment"
                (or comment (disambiguation recording) ""))
          (cons "edit-recording.length"
                (aif (or length (recording-length recording))
                     (format-time-period it)
                     ""))
          (cons "edit-recording.edit_note" (or edit-note ""))
          (cons "edit-recording.as_auto_editor" (if auto-edit "1" "0"))

          (cons "edit-recording.artist_credit.names.0.artist.name"
                (name (artist nc)))
          (cons "edit-recording.artist_credit.names.0.name"
                (or (name nc) (name (artist nc))))
          (cons "edit-recording.artist_credit.names.0.join_phrase" "")
          (cons "edit-recording.artist_credit.names.0.artist.id"
                (princ-to-string (get-db-row (artist nc)))))))

(defun track-to-recording-parameters (track recording &optional note)
  (unless (or (artist-credit track) (artist-credit recording))
    (error "Track ~A has no artist credit." track))

  (edit-recording-parameters recording
                             :edit-note note
                             :auto-edit t
                             :ac (artist-credit track)
                             :title (title track)))

(defun recording-edit (recording parameters)
  (expect-302
    (drakma:http-request (get-edit-url recording)
                         :method :post
                         :parameters parameters
                         :cookie-jar *cookie-jar*))
  (forget-cached recording)
  (values))

(defun rename-recording (track recording &optional note)
  (recording-edit
   recording (track-to-recording-parameters track recording note)))

(defun set-recording-artist (artist recording &key note rename)
  (recording-edit recording
                  (edit-recording-parameters
                   recording :edit-note note :auto-edit t
                   :ac (make-artist-credit artist :name rename))))

(defun set-recording-from-track-name (track recording)
  (format t "Working on track ~D: ~A~%"
          (pos track) (or (title track)
                          (concatenate 'string "(R) " (title recording))))
  (unless (titles-agree-p track recording)
    (format t "  - Title~%    ~A~%   to~%    ~A~%"
            (title recording) (title track)))
  (unless (artist-credits-agree-p track recording)
    (format t "  - Artist~%    ~A~%   to~%    ~A~%"
            (artist-credit-string (artist-credit recording))
            (artist-credit-string (artist-credit track))))
  (finish-output)

  (rename-recording track recording
                    "Taking title and artists from release track.")

  (format t "Done.~%")
  (finish-output)
  (forget-cached recording))

(defun each-track (fun release &key pred)
  "Call (FUN track rec) for each track/recording pair. If PRED is supplied, only
pairs for which PRED is true are used."
  (skippable-each (lambda (pair)
                    (destructuring-bind (track recording) pair
                      (funcall fun track recording)))
                  (if pred
                      (remove-if-not
                       (lambda (pair)
                         (funcall pred (first pair) (second pair)))
                       (track-recording-pairs release))
                      (track-recording-pairs release))
                  :noun "track"))

(defun track-names-to-recordings (release &key force)
  (unwind-protect
       (each-track #'set-recording-from-track-name
                   release
                   :pred (lambda (track recording)
                           (recording-needs-renaming-p track recording force)))
    (clear-cache))
  (values))

(defun csg-split-release-artists (release)
  "Returns (VALUES COMPOSERS PERFORMERS), split by the semicolon as specified in
the CSG."
  (do* ((ncs (name-credits (artist-credit release)) (cdr ncs))
        (composers nil (cons (car ncs) composers)))
       ((null ncs)
        (error "No semicolon-terminated NC found."))
    (when (find #\; (join-phrase (first ncs)))
      (return
        (values
         (reverse (cons (make-name-credit (artist (first ncs))
                                          :title (name (first ncs)))
                        composers))
         (rest ncs))))))

(defun set-csg-recording-artists (release)
  "Go through the recordings in RELEASE and set each one's artist to the
performing artists on the release."
  (multiple-value-bind (composers performers)
      (csg-split-release-artists release)
    (declare (ignore composers))
    (unless (= 1 (length performers))
      (error "Can only cope with a single performer at the moment."))
    (unwind-protect
         (each-track
          (lambda (track recording)
            (format t "Track ~A: \"~A\". "
                    (pos track)
                    (shortened-string (or (title track) (title recording))
                                      :max-length 50))
            (force-output)

            (set-recording-artist (artist (first performers))
                                  recording
                                  :note "CSG recording artist = performer"
                                  :rename (name (first performers)))
         
            (format t "done~%")
            (force-output))
          release)
      (clear-cache))))
