(defpackage mbedit-asd
  (:use :cl :asdf))
(in-package :mbedit-asd)

(defsystem mbedit
    :depends-on (:drakma :alexandria :mbcl)
    :components
    ((:file "package")
     (:file "config" :depends-on ("package"))
     (:file "login" :depends-on ("package" "config"))))
