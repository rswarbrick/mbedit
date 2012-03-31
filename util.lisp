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

(defun last-word (string)
  "Find the last word (separated by spaces) of STRING."
  (let ((pos (position #\Space string :from-end t)))
    (if pos (subseq string (1+ pos)) string)))

(defmacro do-skippable (noun &body body)
  "Run BODY inside a restart-case that allows us to repeat if something goes
wrong or skip execution if the user decides it isn't going to work."
  (let ((repeat (gensym)) (output (gensym)))
    `(loop
        (let ((,repeat nil)
              (,output nil))
          (restart-case
              (setf ,output
                    (progn ,@body))
          
            (skip-this-elt ()
              :report (lambda (stream)
                        (format stream "Skip this ~A" ,noun))
              (return))

            (try-again ()
              :report "Try again"
              (setf ,repeat t)))
          (unless ,repeat (return ,output))))))

(defun skippable-each (fun sequence &key (noun "element"))
  "Call (FUN ELT) for each ELT in SEQUENCE. This is done in a RESTART-CASE which
allows the user to retry or skip an element if it's causing an error."
  (map nil (lambda (elt) (do-skippable noun (funcall fun elt))) sequence))
