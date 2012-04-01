(in-package :mbedit)

(defun recording-dateable-relations (recording)
  (mapcar (lambda (reln) (get-rowed-relation recording reln))
          (append (relations-of-type recording :class 'work)
                  (relations-of-type recording :class 'artist))))

(defun recording-periods (recording)
  "Return a list of the periods associated to the making of this recording."
  (map-filter (lambda (rel)
                (let ((begin (begin rel))
                      (end (end rel)))
                  (when (or begin end)
                    (list (and begin (parse-date-string begin))
                          (and end (parse-date-string end))))))
              (recording-dateable-relations recording)))

(defun periods-agree-p (periods)
  "Returns true if the periods are exactly identical to one another."
  (every (lambda (x)
           (date= (first x) (first (first periods)))
           (date= (second x) (second (first periods))))
         (cdr periods)))

(defun date-relations (recording begin end &key force auto-edit edit-note)
  "Set the dateable relations on RECORDING to the given dates. If FORCE, ignore
conflicting dates."
  (unless (and (typep begin 'date) (typep end 'date))
    (error "BEGIN and END must be MBCL:DATEs."))
  (let ((relations (recording-dateable-relations recording)))
    (unless (or force
                (periods-agree-p (cons (list begin end)
                                       (recording-periods recording))))
      (error "Recording periods don't all agree for this recording."))

    (dolist (rel relations)
      (unless (and (begin rel) (date= (parse-date-string (begin rel)) begin))
        (actually-date-relation recording rel begin end auto-edit edit-note)))))

(defun actually-date-relation (recording relation begin end
                               auto-edit edit-note)
  "Actually set the relationship's date."
  (handler-case
      (edit-relation recording relation
                     :begin begin :end end
                     :auto-edit auto-edit :edit-note edit-note)
    (not-dateable (c)
      (declare (ignore c))
      (values)))
  (values))

(defun date-some-recordings (recordings begin end &key force auto-edit edit-note)
  "Takes a list of recordings (consider using GET-RELEASE-RECORDINGS) and dates
and tags them. Checks for incompatible dates unless FORCE is given."
  (dolist (rec recordings)
    (format t "Recording '~A'... " (title rec))
    (force-output)
    (date-relations rec begin end
                    :force force :auto-edit auto-edit :edit-note edit-note)
    (format t "done.~%")
    (force-output)))

(defun copy-some-recording-dates (recordings &key auto-edit edit-note)
  "For each recording in RECORDINGS, this checks whether there is any date set
on any AR from it. If so (and the dates that are set agree) it copies the date
to the other dateable ARs."
  (dolist (rec recordings)
    (format t "Recording '~A'... " (title rec))
    (force-output)
    (let ((periods (recording-periods rec)))
      (cond
        ((not (periods-agree-p periods))
         (format t "~%  [EE] Recording periods do not agree.~%"))
        ((not periods)
         (format t "~%  [EE] No recording dates.~%"))
        (t
         (date-relations rec (first (first periods)) (second (first periods))
                         :auto-edit auto-edit :edit-note edit-note)
         (format t "done.~%")))
      (force-output))))

(defun copy-release-recording-dates (release)
  "Calls COPY-SOME-RECORDING-DATES on the recordings on the given release."
  (copy-some-recording-dates (pl-as-list (recordings release))
                             :auto-edit t
                             :edit-note "Copying dates between ARs."))

;; Example usage:
;;
;; (date-some-recordings
;;  (get-release-recordings
;;   (url-to-object "http://musicbrainz.org/release/8c9949c6-6674-40df-b63d-7e6034599819")
;;   '(1 9 1))
;;  (parse-date-string "2008-06")
;;  (parse-date-string "2008-06")
;;  :force t
;;  :edit-note "See edit 16151803")
;;
;; And, now I know it works, I should probably also set auto-edit.
