
(asdf:defsystem neomacs
  :version "0.0.1"
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :maintainer "Qiantan Hong <qhong@alum.mit.edu>"
  :license "GPLv3+"
  :description "Structural Lisp Environment"
  :serial t
  :components ((:file "packages")
               (:file "default-value")
               (:file "dom")
               (:file "command")
               (:file "pos-marker")
               (:file "defstyle")
               (:file "plump-hack")
               (:file "mode")
               (:file "keymap")
               (:file "buffer")
               (:file "motion")
               (:file "edit")
               (:file "range")
               (:file "frame")
               (:file "command-loop")
               (:file "minibuffer")
               (:file "undo")
               (:file "ceramic")
               #+nil (:file "manual")
               (:file "syntax")
               (:file "modes/lists")
               (:file "modes/occur")
               (:file "modes/file")
               (:file "modes/text")
               (:file "modes/prog")
               (:file "modes/completion")
               (:file "modes/lisp")
               (:file "modes/html-doc")
               (:file "modes/web")
               (:file "modes/web-hints")
               (:file "start"))
  :depends-on (:lwcells
               :ceramic
               :str
               :dynamic-mixins
               :parenscript
               :plump
               :plump-sexp
               :lass
               :spinneret
               :metabang-bind
               :cl-containers
               :quri
               :trivial-types
               :local-time
               :sb-concurrency))
