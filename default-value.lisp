(in-package #:neomacs)

(defmacro default (slot) `(get ,slot 'default))

(defclass default-mixin () ())

(defmethod slot-unbound ((class t) (instance default-mixin) slot-name)
  (default slot-name))

(defmacro define-class (name super-classes slots &rest options)
  "`defclass' variant that support default values.

- Automatically inherits from `default-mixin'.

- All slots are defined with accessor methods of the same name.

- SLOTS accept :default parameter, which is used to initialize its
  default value."
  (let (defaults)
    `(progn
       (defclass ,name ,super-classes
         ,(mapcar (lambda (slot-def)
                    (setq slot-def (copy-list (uiop:ensure-list slot-def)))
                    (let ((default-form (getf (cdr slot-def) :default '%unbound)))
                      (unless (eq default-form '%unbound)
                        (push `(setf (default ',(car slot-def))
                                     ,default-form)
                              defaults)
                        (remf (cdr slot-def) :default)))
                    (unless (getf (cdr slot-def) :accessor)
                      (alex:appendf (cdr slot-def) (list :accessor (car slot-def))))
                    slot-def)
           slots)
         ,@options)
       ,@(nreverse defaults))))
