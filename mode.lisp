(in-package #:neomacs)

(defvar *modes* nil)

(defclass mode (standard-class)
  ((commands :initform nil :accessor commands)
   (keymap :initform (make-keymap nil) :accessor keymap)
   (lighter
    :initform nil :accessor lighter :type string
    :documentation
    "String displayed in header to indicate this mode is enabled."))
  (:documentation "Metaclass for modes."))

(macrolet ((define-symbol-accessors (accessor)
             `(progn
                (defmethod ,accessor ((object t)))
                (defmethod ,accessor ((name symbol))
                  (,accessor (find-class name)))
                (defmethod (setf ,accessor)
                    (new-val (name symbol))
                  (setf (,accessor (find-class name)) new-val)))))
  (define-symbol-accessors commands)
  (define-symbol-accessors keymap)
  (define-symbol-accessors lighter))

(defmethod sb-mop:validate-superclass
    ((class mode) (super standard-class))
  t)

(defun default-lighter (class)
  "Compute the default lighter string for CLASS."
  (let ((name (symbol-name (class-name class))))
    (when (sera:string-suffix-p "-MODE" name)
      (setf name (subseq name 0 (- (length name) 5))))
    (string-capitalize name)))

(defmethod shared-initialize :after
    ((class mode) slot-names
     &key toggler lighter documentation)
  (declare (ignore slot-names))
  (pushnew (class-name class) *modes*)
  (flet ((safe-car (form)
           (when form
             (if (cdr form)
                 (error "~a has more than one element."
                        form)
                 (car form)))))
    (when (safe-car toggler)
      (let ((name (string-downcase (symbol-name
                                    (class-name class)))))
        (eval `(define-command ,(class-name class) ()
                 ,documentation
                 (message "~a ~:[disabled~;enabled~]"
                          ,name
                          (toggle ',(class-name class)))))))
    (setf (lighter class)
          (or (safe-car lighter)
              (default-lighter class)))))

(defmacro define-mode
    (name super-modes slots &rest options)
  `(define-class ,name ,super-modes ,slots
     (:metaclass mode)
     ,@options))
