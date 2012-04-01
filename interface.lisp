(in-package :mbedit)

(defun get-release-recordings (release &rest tracklist)
  "Return the recordings of RELEASE in positions given by TRACKLIST. The latter
is a list consisting of elements '(a b c), which means tracks A - B (starting at
1) on disc C (also starting at 1). If there is only one medium on the release
and C is not given, it is taken to be 1. Instead of a triple, an element of
TRACKLIST can be a single number (as long as there is only one medium). In this
case, it denotes that track on the first and only medium."
  (let ((track-lists
         (mapcar (lambda (medium)
                   (cons (pos medium) (pl-as-list (track-list medium))))
                 (pl-as-list (medium-list release)))))
    (mapcan
     (lambda (triple)
       (when (integerp triple) (setf triple (list triple triple)))
       (destructuring-bind (a b &optional c) triple
         (unless c
           (if (= 1 (length track-lists))
               (setf c 1)
               (error "Ambiguous triple: ~A (more than one medium)" triple)))
         (when (< b a)
           (error "Invalid range of tracks: [~A, ~A]." a b))

         (let ((tracks (cdr (assoc c track-lists))))
           (unless tracks
             (error "No such medium: ~A" c))
           (unless (<= 1 a b (length tracks))
             (error "Can't find tracks ~A to ~A on medium ~A (has ~A tracks)"
                    a b c (length tracks)))
           (mapcar #'recording (subseq tracks (1- a) b)))))
     tracklist)))
