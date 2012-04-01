(defpackage mbedit-asd
  (:use :cl :asdf))
(in-package :mbedit-asd)

(defsystem mbedit
    :depends-on (:drakma :alexandria :mbcl :anaphora :split-sequence :yason
                 :closure-html :cxml-stp :cl-ppcre)
    :components
    ((:file "package")
     (:file "config" :depends-on ("package"))
     (:file "login" :depends-on ("package" "config"))
     (:file "recording-to-work" :depends-on ("package" "login" "util"))
     (:file "json" :depends-on ("package"))
     (:file "browser" :depends-on ("package"))
     (:file "tracks-to-recordings"
            :depends-on ("package" "login" "recording-to-work" "json"))
     (:file "add-work" :depends-on ("package" "login" "recording-to-work"))
     (:file "interface" :depends-on ("package"))
     (:file "util" :depends-on ("package"))
     (:file "relation-types" :depends-on ("package" "login" "util"))
     (:file "date-recordings"
            :depends-on ("package" "edit-relation" "util"))
     (:file "edit-relation"
            :depends-on ("package" "relation-types" "json"))
     (:file "un-orchestrate"
            :depends-on ("package" "edit-relation" "util"))))
