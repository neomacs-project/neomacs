
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
               (:file "mode")
               (:file "keymap")
               (:file "buffer")
               (:file "command-loop")
               (:file "motion")
               (:file "edit")
               (:file "range")
               (:file "frame")
               (:file "minibuffer")
               (:file "undo")
               (:file "ceramic")
               #+nil (:file "manual")
               (:file "syntax")
               (:file "modes/lists")
               (:file "themes")
               (:file "modes/occur")
               (:file "modes/search")
               (:file "modes/file")
               (:file "modes/auto-save")
               (:file "modes/text")
               (:file "modes/prog")
               (:file "modes/completion")
               (:file "modes/lisp")
               (:file "modes/debug")
               (:file "modes/describe")
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
               :sb-concurrency
               :dissect
               :trivial-custom-debugger
               :cl-tld
               :swank
               :swank/exts))
