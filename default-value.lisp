(in-package #:neomacs)

(sera:export-always
    '(default
      default-mixin
      define-class))

(defclass defaultable-class (standard-class) ())

(defmethod sb-mop:validate-superclass
    ((a defaultable-class) (b standard-class))
  t)

(defclass defaultable-direct-slot-definition
    (sb-mop:standard-direct-slot-definition)
  ((default-value)))

(defclass defaultable-effective-slot-definition
    (sb-mop:standard-effective-slot-definition)
  ((default-direct-slot)))

(defmethod sb-mop:direct-slot-definition-class
    ((class defaultable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'defaultable-direct-slot-definition))

(defmethod sb-mop:effective-slot-definition-class
    ((class defaultable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'defaultable-effective-slot-definition))

(defmethod initialize-instance :after
    ((slot defaultable-direct-slot-definition)
     &key (default nil default-p))
  (when default-p
    (setf (slot-value slot 'default-value)
          (eval default))))

(defmethod sb-mop:compute-effective-slot-definition
    ((class defaultable-class) name dslotds)
  (lret ((slot (call-next-method)))
    (setf (slot-value slot 'default-direct-slot)
          (find-if (alex:rcurry #'slot-boundp 'default-value)
                   dslotds))))

(defmethod slot-unbound ((class defaultable-class) object slot-name)
  ;; TODO: eliminate the search somehow?
  (let ((slot (find slot-name (sb-mop:class-slots class)
                    :key #'sb-mop:slot-definition-name)))
    (if-let (ds (slot-value slot 'default-direct-slot))
      (slot-value ds 'default-value)
      (call-next-method))))

(defmacro default (class-name slot-name)
  `(slot-value (find ,slot-name
                     (sb-mop:class-direct-slots (%find-class ,class-name))
                     :key #'sb-mop:slot-definition-name)
               'default-value))

(defmacro define-class (name super-classes slots &rest options)
  "`defclass' variant.

- All slots are defined with accessor methods of the same name."
  `(defclass ,name ,super-classes
     ,(mapcar (lambda (slot-def)
                (setq slot-def (copy-list (uiop:ensure-list slot-def)))
                (unless (getf (cdr slot-def) :accessor)
                  (alex:appendf (cdr slot-def) (list :accessor (car slot-def))))
                slot-def)
       slots)
     ,@options))

;; Adapted from dynamic-mixins by Ryan Pavlik
;; BSD-2-Clause

(defvar *dynamic-mix-classes* (make-hash-table :test 'equal))

(defclass mixin-class (defaultable-class)
  ((classes :initform nil :initarg :classes)))

(defmethod validate-superclass ((class mixin-class) (super standard-class)) t)

(defmethod print-object ((o mixin-class) stream)
  (with-slots (classes) o
    (print-unreadable-object (o stream :identity t)
      (format stream "~S ~S"
              (or (class-name o) 'mixin-class)
              (mapcar #'class-name classes)))))

(defclass mixin-object () ())

(defstruct mix-list (list nil))

(defun %find-class (name-or-class)
  (etypecase name-or-class
    (symbol (find-class name-or-class))
    (sb-mop:class name-or-class)))

(defun %mix (object-or-class &rest class-list)
  "Create a MIX-LIST for MAKE-INSTANCE.  The first element may be an
instance; further elements must be class names or classes."
  (let ((class0 (typecase object-or-class
                  (symbol (list (find-class object-or-class)))
                  (mixin-object
                   (slot-value (class-of object-or-class) 'classes))
                  (t (list (class-of object-or-class))))))
    (make-mix-list
     :list (remove-duplicates
            (append (mapcar #'%find-class class-list)
                    class0)))))

(defun mix (&rest classes)
  (make-mix-list :list (remove-duplicates (mapcar #'%find-class classes))))

(defun set-superclasses (class list)
  (reinitialize-instance class :direct-superclasses list))

(defun define-mixin (mix-list)
  (let ((new-class (make-instance 'mixin-class
                                  :classes (mix-list-list mix-list))))
    (handler-case
        (progn
          (set-superclasses new-class (list* (find-class 'mixin-object)
                                             (mix-list-list mix-list))))
      (error (e)
        (set-superclasses new-class nil)
        (error e)))
    (setf (gethash (mix-list-list mix-list) *dynamic-mix-classes*)
          new-class)))

(defun ensure-mixin (mix-list)
  (if (cdr (mix-list-list mix-list))
      (if-let ((class (gethash (mix-list-list mix-list)
                               *dynamic-mix-classes*)))
        class
        (define-mixin mix-list))
      (car (mix-list-list mix-list))))

(defun ensure-mix (object &rest classes)
  (let ((new-class (ensure-mixin (apply #'%mix object classes))))
    (change-class object new-class)))

(defun delete-from-mix (object &rest classes)
  (if (typep object 'mixin-object)
      (let* ((classes (mapcar #'%find-class classes))
             (old-classes (slot-value (class-of object) 'classes))
             (new-classes (remove-if (lambda (x) (member (%find-class x) classes))
                                     old-classes))
             (new-class (if (cdr new-classes)
                            (ensure-mixin (apply #'mix new-classes))
                            (car new-classes))))
        (change-class object new-class))
      object))

(defmethod make-instance ((items mix-list) &rest initargs &key &allow-other-keys)
  (apply #'make-instance (ensure-mixin items) initargs))
