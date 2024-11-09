(in-package #:neomacs)

(sera:export-always
    '(define-command call-interactively))

(defvar *commands* nil "List of known commands.")

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
  "Define a command with NAME.

ARGS has the form `{:option OPTION}* LAMBDA-LIST FORM*'.  This is
like `(defun LAMBDA-LIST FORM*)' besides supporting extra options:

:mode MODE-OR-MODES: The command is made avaliable in
MODE-OR-MODES. MODE-OR-MODES can either be a list or a single
symbol. If this options is not provided, the command is avaliable
globally.

:interactive INTERACTIVE: INTERACTIVE should evaluate to a function
which takes zero arguments. When called, it should return a list of
arguments which can be supplied to the command. The command loop and
`call-interactive' call this function to compute arguments for the
command."
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
  "Call SYMBOL-OR-FUNCTION like interactively from the command loop.

If SYMBOL-OR-FUNCTION is a symbol, this provides argument according to
its `interactive' symbol property (set by :interactive options of
`define-command')."
  (etypecase symbol-or-function
    (symbol (apply symbol-or-function
                   (funcall (get symbol-or-function
                                 'interactive))))
    (function (funcall symbol-or-function))))
