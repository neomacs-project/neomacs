(in-package #:neomacs)

(defvar *commands* nil)

(defmacro define-command (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)
     (pushnew ',name *commands*)
     ',name))
