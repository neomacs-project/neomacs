(in-package :neomacs)

(defun get-style (symbol)
  (let ((style (get symbol 'style '%unbound)))
    (if (eql style '%unbound)
        (warn "Style for ~a is unbound." symbol)
        (cell-ref style))))

(defun substitute-inherits (form)
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

(defmacro defstyle (name spec &optional doc)
  `(progn
     (setf (cell-ref (get ',name 'style)) ,spec)
     (setf (documentation ',name 'style) ,doc)
     ',name))

(defun css-cell (symbol)
  (or (get symbol 'css)
      (setf (get symbol 'css)
            (cell (apply #'styled-css (get-style symbol))))))
