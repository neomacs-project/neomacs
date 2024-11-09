(in-package :neomacs)

(sera:eval-always
  '(defstyle))

(defun get-style (symbol)
  (let ((style (get symbol 'style '%unbound)))
    (if (eql style '%unbound)
        (warn "Style for ~a is unbound." symbol)
        (cell-ref style))))

(defun substitute-inherits (form)
  "Expand :inherit keys in style spec FORM."
  (labels ((process (form)
             (typecase form
               (cons
                (iter (with rest = form)
                  (while rest)
                  (for item = (pop rest))
                  (if (eql item :inherit)
                      (let ((value (pop rest)))
                        (etypecase value
                          (symbol
                           (appending (process (get-style value))))
                          (cons (appending
                                 (mapcan
                                  (lambda (v)
                                    (process (get-style v)))
                                  value)))))
                      (collect (process item)))))
               (t form))))
    (process form)))

(defun hoist-special-blocks (forms)
  "Expand blocks started with (:append ...) in style spec FORM."
  (labels ((process (form)
             (bind (((selector . body) form)
                    results)
               (cons
                (cons selector
                      (iter (for c in body)
                        (match c
                          ((cons (cons :append s) rest)
                           (push `((:and ,selector . ,s) . ,rest)
                                 results))
                          (_ (collect c)))))
                (nreverse results)))))
    (mapcan #'process forms)))

(defun styled-css (&rest blocks)
  (apply #'lass:compile-and-write
         (hoist-special-blocks (mapcar #'substitute-inherits blocks))))

(defmethod documentation (symbol (type (eql 'style)))
  (get symbol 'style-doc))

(defmethod (setf documentation) (new-val symbol (type (eql 'style)))
  (setf (get symbol 'style-doc) new-val))

(defvar *styles* nil "List of all known styles.")

(defun initialize-style (symbol spec)
  (pushnew symbol *styles*)
  (setf (cell-ref (get symbol 'style)) spec
        (get symbol 'standard-style) (copy-tree spec)))

(defmacro defstyle (symbol spec &optional doc)
  "Define a style named by SYMBOL."
  `(progn
     (initialize-style ',symbol ,spec)
     (setf (documentation ',symbol 'style) ,doc)
     (sera:export-always ',symbol)
     ',symbol))

(defun css-cell (symbol)
  "Return cell that stores the compiled CSS of the style named by SYMBOL.

Can be `cell-ref'ed to get the up-to-date CSS string for the style
named by SYMBOL."
  (or (get symbol 'css)
      (setf (get symbol 'css)
            (cell (apply #'styled-css (get-style symbol))))))
