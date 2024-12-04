(asdf:defsystem neomacs
  :version "0.1.1"
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
               (:file "keymap")
               (:file "mode")
               (:file "buffer")
               (:file "ceramic")
               (:file "command-loop")
               (:file "motion")
               (:file "edit")
               (:file "range")
               (:file "gray")
               (:file "frame")
               (:file "minibuffer")
               (:file "undo")
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
               (:file "modes/web-adblock")
               (:file "intl/simple")
               (:file "start"))
  :depends-on (:lwcells
               :ceramic
               :str
               :parenscript
               :lass
               :spinneret
               :metabang-bind
               :cl-containers
               :quri
               :dexador
               :trivial-types
               :local-time
               :sb-concurrency
               :dissect
               :trivial-custom-debugger
               :cl-tld
               :osicat
               :swank
               :swank/exts
               :trivial-package-local-nicknames))

(asdf:defsystem neomacs/app
  :defsystem-depends-on (:deploy)
  :depends-on (:unix-opts
               :neomacs
               (:feature (:not :windows) :neomacs/term))
  :build-operation "deploy-op"
  :build-pathname "neomacs"
  :entry-point "neomacs::entry"
  :serial t
  :components ((:file "asdf-bundler")
               (:file "deploy")))

(asdf:defsystem neomacs/term
  :defsystem-depends-on (:cffi-toolchain)
  :license "GPLv3+"
  :components
  ((:module "term"
    :serial t
    :components
    ((:file "package")
     (:file "term")
     (:c-file "term-helper"))))
  :depends-on (:neomacs :3bst))
