(in-package #:neomacs)

(sera:export-always
    '(define-command))

(defvar *commands* nil)

(defmethod commands ((name (eql :global)))
  *commands*)

(defmethod (setf commands) (new-val (name (eql :global)))
  (setf *commands* new-val))

(defun split-args (args)
  "Split ARGS into a preceding plist and the rest."
  (let (options)
    (iter
      (while args)
      (while (keywordp (car args)))
      (push (car args) options)
      (push (cadr args) options)
      (setq args (cddr args)))
    (values (nreverse options) args)))

(defmacro define-command (name &rest args)
  (bind (((:values options args) (split-args args))
         ((lambda-list . body) args)
         (modes (uiop:ensure-list (getf options :mode :global)))
         (interactive (getf options :interactive '(lambda () nil))))
    `(progn
       (sera:export-always ',name)
       (defun ,name ,lambda-list ,@body)
       ,@ (iter (for m in modes)
            (collect `(pushnew ',name (commands ',m))))
       (setf (get ',name 'modes) ',modes)
       (setf (get ',name 'interactive) ,interactive)
       ',name)))

(defun call-interactively (symbol-or-function)
  (etypecase symbol-or-function
    (symbol (apply symbol-or-function
                   (funcall (get symbol-or-function
                                 'interactive))))
    (function (funcall symbol-or-function))))
