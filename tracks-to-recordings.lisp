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

(defun pairs-to-rename (release &optional force)
  (remove-if-not (lambda (pair) (recording-needs-renaming-p
                                 (first pair) (second pair) force))
                 (track-recording-pairs release)))

(defun get-edit-url (object)
  (format nil "~A~A/~A/edit" *mb-root-url* (table-name object) (id object)))

(defun edit-recording-parameters (track recording &optional note)
  (unless (or (artist-credit track) (artist-credit recording))
    (error "Track ~A has no artist credit." track))

  (let* ((name-credits (name-credits (or (artist-credit track)
                                         (artist-credit recording))))
         (nc (first name-credits)))
    (unless (and (= 1 (length name-credits)) (null (join-phrase nc)))
      (error "Artist credit too complicated for me!"))

    (list (cons "edit-recording.name" (title track))
          (cons "edit-recording.comment" (or (disambiguation recording) ""))
          (cons "edit-recording.length" (aif (recording-length recording)
                                             (format-time-period it)
                                             ""))
          (cons "edit-recording.edit_note" (or note ""))
          (cons "edit-recording.as_auto_editor" "1")

          (cons "edit-recording.artist_credit.names.0.artist.name"
                (name (artist nc)))
          (cons "edit-recording.artist_credit.names.0.name"
                (or (name nc) (name (artist nc))))
          (cons "edit-recording.artist_credit.names.0.join_phrase" "")
          (cons "edit-recording.artist_credit.names.0.artist.id"
                (princ-to-string (get-db-row (artist nc)))))))

(defun rename-recording (track recording &optional note)
  (expect-302
    (drakma:http-request
     (get-edit-url recording)
     :method :post
     :parameters (edit-recording-parameters track recording note)
     :cookie-jar *cookie-jar*))
  (values))

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

(defun track-names-to-recordings (release &key force)
  (unwind-protect
       (let ((skip-next-track nil))
         (dolist (pair (pairs-to-rename release force))
           (restart-case
               (if skip-next-track
                   (setf skip-next-track nil)
                   (destructuring-bind (track recording) pair
                     (set-recording-from-track-name track recording)))
             (skip-this-track ()
               :report "Skip this track"
               (format t "Skipped.~%")
               (finish-output)
               (setf skip-next-track t))
             (try-again ()
               (format t "Retry.~%")
               (finish-output)
               :report "Try again"))))
    (forget-cached release))
  (values))
