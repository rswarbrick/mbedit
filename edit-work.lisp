(in-package :mbedit)

(defmethod edit-work-url ((work work))
  (format nil "~Awork/~A/edit" *mb-root-url* (id work)))

(defmethod edit-work-parameters ((work work) name comment edit-note iswc)
  (when (work-type work)
    (error "Work types not yet supported."))
  `(("edit-work.name" . ,(or name (title work)))
    ("edit-work.comment" . ,(or comment ""))
    ;; This isn't the data disaster it appears to be! When I first try to parse
    ;; a work with an ISWC, MBCL will fall over and I can fix this.
    ("edit-work.iswc" . ,(or iswc ""))
    ("edit-work.type_id" . "")
    ("edit-work.edit_note" . ,(or edit-note ""))
    ("edit-work.as_auto_editor" . "1")))

(defun edit-work (work &key name disambiguation edit-note iswc)
  (expect-302
    (drakma:http-request
     (edit-work-url work)
     :method :post
     :parameters (edit-work-parameters work name disambiguation edit-note iswc)
     :cookie-jar *cookie-jar*))
  (forget-cached work))

(defun subworks (work)
  "Subparts of a work get a work relation with type \"parts\" and direction nil,
apparently. I suppose we may as well allow forwards too. This returns all
immediate subworks."
  (mapcar #'target
          (remove-if-not
           (lambda (rel)
             (and (string= (relation-type rel) "parts")
                  (member (direction rel) '(nil "forwards") :test #'equal)))
           (relations-of-type work :class 'work))))

(defun work-tree-leaves (work)
  "Return all the leaves of the work tree below (and including) WORK."
  (aif (subworks work)
       (mapcan #'work-tree-leaves it)
       (list work)))
