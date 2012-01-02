(defpackage mbedit-asd
  (:use :cl :asdf))
(in-package :mbedit-asd)

(defsystem mbedit
    :depends-on (:drakma :alexandria :mbcl :anaphora :split-sequence)
    :components
    ((:file "package")
     (:file "config" :depends-on ("package"))
     (:file "login" :depends-on ("package" "config"))
     (:file "recordings-to-tracks" :depends-on ("package" "login"))
     (:file "add-work" :depends-on ("package" "login" "recordings-to-tracks"))))
