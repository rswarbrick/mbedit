(in-package :mbedit)

(defvar *browser-path* #P"/usr/bin/xdg-open"
  "The browser name")

(defun open-url (url)
  (sb-ext:run-program *browser-path* (list url) :wait nil)
  (values))

(defun browse-object (mb-object)
  (open-url (page mb-object)))
