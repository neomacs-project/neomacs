(in-package :neomacs)

(sera:export-always
  '(defstyle defsheet css-cell set-style *styles*))

(defun get-style (symbol)
  (let ((style (get symbol 'style '%unbound)))
    (if (eql style '%unbound)
        (warn "Style for ~a is unbound." symbol)
        (cell-ref style))))

(defun substitute-inherits (form)
  "Expand :inherit options in style spec FORM."
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
  "Define a style named by SYMBOL.

This also exports SYMBOL from current package."
  `(progn
     (initialize-style ',symbol ,spec)
     (setf (documentation ',symbol 'style) ,doc)
     (sera:export-always ',symbol)
     ',symbol))

(defmacro defsheet (symbol spec &optional doc)
  "Define a style sheet named by SYMBOL.

This also exports SYMBOL from current package."
  `(progn
     (setf (get ',symbol 'sheet) t)
     (defstyle ,symbol ,spec ,doc)))

(defun css-cell (symbol)
  "Return the cell that stores the compiled CSS of the style sheet named
by SYMBOL.

Can be `cell-ref'ed to get the up-to-date CSS string for the style
named by SYMBOL."
  (unless (get symbol 'sheet)
    (error "~a does not name a style sheet" symbol))
  (or (get symbol 'css)
      (setf (get symbol 'css)
            (cell (apply #'styled-css (get-style symbol))))))

(defun set-style (&rest bindings)
  "Set styles or style sheets according to BINDINGS.

BINDINGS should be of the form {SYMBOL SPEC}*, and style or style
sheet named by each SYMBOL is set to SPEC.

Example: (set-style 'default '(:font-family \"sans-serif\")
    'bold '(:font-weight 900))

SYMBOL is allowed to take some special value:

:native-theme -- SPEC should be one of \"system\",\"dark\" and \"light\". This sets the color theme used by the renderer."
  (iter (for (symbol spec) on bindings by #'cddr)
    (case symbol
      ((:native-theme)
       (evaluate-javascript
        (format nil "nativeTheme.themeSource = ~S" spec)
        :global))
      (t (setf (cell-ref (get symbol 'style)) spec)))))

(setf (get :native-theme 'standard-style) "light")

;; Adapted from Gogh schemes Everforest light medium
;; https://gogh-co.github.io/Gogh/

(defstyle ansi-black '(:color "#d3c6aa"))
(defstyle ansi-red '(:color "#f85552"))
(defstyle ansi-green '(:color "#8da101"))
(defstyle ansi-yellow '(:color "#dfa000"))
(defstyle ansi-blue '(:color "#3a94c5"))
(defstyle ansi-magenta '(:color "#df69ba"))
(defstyle ansi-cyan '(:color "#35a77c"))
(defstyle ansi-white '(:color "#000"))
(defstyle ansi-bright-black '(:color "#dfddc8"))
(defstyle ansi-bright-red '(:color "#e67e80"))
(defstyle ansi-bright-green '(:color "#a7c080"))
(defstyle ansi-bright-yellow '(:color "#dbbc7f"))
(defstyle ansi-bright-blue '(:color "#7fbbb3"))
(defstyle ansi-bright-magenta '(:color "#d699b6"))
(defstyle ansi-bright-cyan '(:color "#83c092"))
(defstyle ansi-bright-white '(:color "#5c6a72"))
