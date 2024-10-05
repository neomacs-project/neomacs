(in-package #:neomacs)

(defmacro default (class slot)
  `(getf (get ,class 'defaults) ,slot))

(defclass default-mixin () ())

(defmethod slot-unbound (class (instance default-mixin) slot-name)
  (when-let (class (class-name class))
    (default class slot-name)))

(defmacro define-class (name super-classes slots &rest options)
  "`defclass' variant that support default values.

- Automatically inherits from `default-mixin'.

- All slots are defined with accessor methods of the same name.

- SLOTS accept :default parameter, which is used to initialize its
  default value.

- SLOTS accept :hook parameter, which is used to initialize
  its default value, and generate an initform which is a hook that has
  a default handler which runs the default hook."
  (let (defaults)
    `(progn
       (defclass ,name ,super-classes
         ,(mapcar (lambda (slot-def)
                    (setq slot-def (copy-list (uiop:ensure-list slot-def)))
                    (when-let (default-form (getf (cdr slot-def) :default))
                      (push `(setf (default ',name ',(car slot-def))
                                   ,default-form)
                            defaults)
                      (remf (cdr slot-def) :default))
                    (when-let (default-hook-form (getf (cdr slot-def) :hook))
                      (push `(setf (default ',name ',(car slot-def))
                                   ,default-hook-form)
                            defaults)
                      (remf (cdr slot-def) :hook)
                      (alex:nconcf
                       (cdr slot-def)
                       (list :initform `(lret ((hook ,default-hook-form))
                                          (hooks:add-hook hook (make-default-handler
                                                                ',name ',(car slot-def)))))))
                    (unless (getf (cdr slot-def) :accessor)
                      (alex:appendf (cdr slot-def) (list :accessor (car slot-def))))
                    slot-def)
           slots)
         ,@options)
       ,@(nreverse defaults))))

(defun make-default-handler (class slot)
  (make-instance
   'hooks:handler
   :name 'default
   :fn (lambda (&rest args)
         (apply #'hooks:run-hook (default class slot) args))))
