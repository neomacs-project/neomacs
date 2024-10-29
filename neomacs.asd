
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
               (:file "buffer")
               (:file "keymap")
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
               (:file "modes/list-modes")
               (:file "modes/occur-mode")
               (:file "modes/file-mode")
               (:file "modes/text-mode")
               (:file "modes/prog-mode")
               (:file "modes/completion")
               (:file "modes/lisp-mode")
               (:file "modes/html-doc")
               (:file "modes/web-mode")
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
