(asdf:defsystem neomacs
  :version "0.0.1"
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :maintainer "Qiantan Hong <qhong@alum.mit.edu>"
  :license "GPLv3+"
  :description "Structural Editor on Nyxt"
  :serial t
  :components ((:file "packages")
               (:file "configuration")
               (:file "dom")
               (:file "pos-marker")
               (:file "defstyle")
               (:file "undo")
               (:file "neomacs")
               (:file "edit")
               (:file "completion")
               (:file "manual")
               (:file "modes/file-mode")
               (:file "modes/lisp-mode")
               (:file "modes/lisp-file"))
  :depends-on (:lwcells
               :nyxt
               :metabang-bind))
