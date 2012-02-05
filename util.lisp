(in-package :mbedit)

(defun delete-nils (seq)
  "Remove all nils from the sequence and return it. may modify seq."
  (delete-if-not #'identity seq))

(defun map-filter (transform sequence &rest more-sequences)
  "Apply transform to the elements of sequence via a map, yielding a list. then
throw away any nils."
  (delete-nils (apply #'map 'list transform sequence more-sequences)))

(defun date-parameters (name date)
  "Return a list of parameters <name>.year, <name>.month and <name>.day filled
in from DATE (which can be NIL)."
  (mapcar (lambda (suffix accessor)
            (cons (concatenate 'string name "." suffix)
                  (aif (and date (funcall accessor date))
                       (princ-to-string it)
                       "")))
          '("year" "month" "day")
          '(year month day)))

(defun ar-period-parameters (begin end)
  "Make parameters for ar.begin_date and ar.end_date."
  (append (date-parameters "ar.begin_date" begin)
          (date-parameters "ar.end_date" end)))

(defun get-relation-entities (owner relation)
  "Get source and target (the right way around!) for a relation."
  (if (and (direction relation)
             (string= "backward" (direction relation)))
      (values (target relation) owner)
      (values owner (target relation))))
