(in-package #:neomacs)

(define-mode lisp-mode (prog-mode) ()
  (:documentation "Lisp mode.")
  (:hooks auto-completion-mode))

(define-keys lisp-mode
  "tab" 'show-completions
  "M-(" 'wrap-paren
  "M-;" 'wrap-comment
  "M-r" 'lisp-raise
  "M-s" 'lisp-splice

  "C-M-x" 'eval-defun
  "C-c C-c" 'compile-defun
  "C-c C-k" 'lisp-compile-file
  "M-p" 'previous-compiler-note
  "M-n" 'next-compiler-note
  "M-." 'goto-definition
  "tab" 'show-completions
  "C-x C-e" 'eval-last-expression
  "C-c C-p" 'eval-print-last-expression)

(defmethod selectable-p-aux ((buffer lisp-mode) pos)
  (if-let (node (node-after pos))
    (and (not (symbol-node-p node))
         (not (and (new-line-node-p node)
                   (non-empty-symbol-node-p (node-before pos)))))
    (not (non-empty-symbol-node-p (node-before pos)))))

(defmethod render-focus-aux ((buffer lisp-mode) pos)
  (match pos
    ((end-pos node)
     (if (non-empty-symbol-node-p node)
         (let ((next (next-sibling node)))
           (cond ((not next)
                  (call-next-method buffer (end-pos (parent node))))
                 (t (call-next-method))))
         (call-next-method)))
    (_ (call-next-method))))

(defmethod block-element-p-aux ((buffer lisp-mode) element)
  (if (equal (tag-name element) "div")
      (not (class-p element "list" "comment"))
      (call-next-method)))

(defmethod check-read-only ((buffer lisp-mode) pos)
  (when (class-p (node-containing pos) "object")
    (error 'element-read-only-error :element (node-containing pos))))

(defmethod on-focus-move progn ((buffer lisp-mode) old new)
  (declare (ignore old))
  (let ((node (node-containing new)))
    (if (or (class-p node "list" "symbol")
            (tag-name-p node "body"))
        (enable 'sexp-editing-mode)
        (disable 'sexp-editing-mode))))

(defgeneric print-dom (object &key &allow-other-keys))

(defun compute-operator (node)
  (when (eql (first-child (parent node)) node)
    ""))

(defun compute-symbol (node)
  (ignore-errors
   (swank::parse-symbol (text (first-child node)))))

(defun compute-symbol-type (node)
  (when-let (symbol (compute-symbol node))
    (cond ((or (and (special-operator-p symbol)
                    (not (member symbol '(function))))
               (member symbol '(declare)))
           "special-operator")
          ((macro-function symbol) "macro")
          ((keywordp symbol) "keyword")
          (t ""))))

(defmethod on-node-setup progn ((buffer lisp-mode) node)
  (set-attribute-function node "operator" 'compute-operator)
  (with-post-command (node 'parent)
    (let ((parent (parent node)))
      (when (symbol-node-p parent)
        (let ((prev (previous-sibling node))
              (next (next-sibling node)))
          (cond ((and (not prev) (not next))
                 (raise-node node))
                ((not prev)
                 (move-nodes node (pos-right node)
                             parent))
                ((not next)
                 (move-nodes node nil (pos-right parent)))
                (t (let ((second-symbol (split-node node)))
                     (move-nodes node (pos-right node)
                                 second-symbol))))))))
  (when (symbol-node-p node)
    (set-attribute-function node "symbol-type" 'compute-symbol-type)))

(defun make-list-node (children)
  (lret ((node (make-instance 'element :tag-name "div")))
    (setf (attribute node "class") "list")
    (iter (for c in children)
      (append-child node c))))

(defun make-atom-node (class text)
  (lret ((node (make-instance 'element :tag-name "span")))
    (setf (attribute node "class") class)
    (when (plusp (length text))
      (append-child node (make-instance 'text-node :text text)))))

(defun list-node-p (node)
  (class-p node "list"))

(defun atom-node-p (node)
  (class-p node "symbol" "string" "object" "comment"))

(defun sexp-node-p (node)
  (class-p node "list" "symbol" "string" "object"))

(defun symbol-node-p (node)
  (class-p node "symbol"))

(defun non-empty-symbol-node-p (node)
  (and (symbol-node-p node) (first-child node)))

(defmethod print-dom ((cons cons) &key)
  (make-list-node (mapcar #'print-dom cons)))

(defmethod print-dom ((null null) &key)
  (make-list-node nil))

(defmethod print-dom ((symbol (eql '%br)) &key)
  (make-new-line-node))

(defmethod print-dom ((symbol symbol) &key)
  (make-atom-node "symbol"
                  (let ((*print-case* :downcase))
                    (prin1-to-string symbol))))

(defmethod print-dom ((string string) &key)
  (make-atom-node "string" string))

(defmethod print-dom ((obj number) &key)
  (make-atom-node "symbol" (prin1-to-string obj)))

(defmethod print-dom ((obj character) &key)
  (make-atom-node "symbol" (prin1-to-string obj)))

(defmethod print-dom ((obj t) &key)
  (lret ((node (make-atom-node "object" (prin1-to-string obj))))
    (setf (attribute node 'presentation) obj)))

(define-command open-paren :mode lisp-mode
  (&optional (marker (focus)))
  (let ((node (make-list-node nil)))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command wrap-paren :mode lisp-mode
  (&optional (pos (focus)))
  (let ((node (make-list-node nil)))
    (setq pos (or (pos-up-ensure pos #'sexp-node-p)
                  (error 'top-of-subtree)))
    (wrap-node pos node)))

(define-command open-string :mode lisp-mode
  (&optional (marker (focus)))
  (let ((node (make-atom-node "string" "")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command open-space :mode lisp-mode
  (&optional (marker (focus)))
  (if (class-p (node-containing marker) "symbol")
      (setf (pos marker) (pos-down (split-node marker)))
      (let ((node (make-atom-node "symbol" "")))
        (insert-nodes marker node)
        (setf (pos marker) (end-pos node)))))

(define-command open-comment :mode lisp-mode
  (&optional (marker (focus)))
  (labels ((cycle-level (n)
             (lret ((n (1+ (mod n 4))))
               (message "Comment Level -> ~a" n))))
    (if-let (node (comment-around marker))
      (setf (attribute node "comment-level")
            (prin1-to-string
             (cycle-level
              (parse-number:parse-number
               (attribute node "comment-level")))))
      (let ((node (make-atom-node "comment" "")))
        (setf (attribute node "comment-level") "1")
        (insert-nodes marker node)
        (setf (pos marker) (end-pos node))))))

(define-command wrap-comment :mode lisp-mode
  (&optional (pos (focus)))
  (let ((node (make-atom-node "comment" "")))
    (setq pos (or (pos-up-ensure pos #'sexp-node-p)
                  (error 'top-of-subtree)))
    (wrap-node pos node)))

(define-command lisp-raise :mode lisp-mode
  (&optional (pos (focus)))
  (setq pos (or (pos-up-ensure pos #'sexp-node-p)
                (error 'top-of-subtree)))
  (raise-node pos))

(define-command lisp-splice :mode lisp-mode
  (&optional (pos (focus)))
  (setq pos (or (pos-up-until pos #'list-node-p)
                (error 'top-of-subtree)))
  (splice-node pos))

(define-mode sexp-editing-mode (lisp-mode) ()
  (:documentation "Editing S-exp."))

(define-keys sexp-editing-mode
  "(" 'open-paren
  "\"" 'open-string
  "space" 'open-space
  ";" 'open-comment)

(defmethod self-insert-aux ((buffer sexp-editing-mode) marker string)
  (cond ((atom-node-p (node-containing marker))
         (insert-nodes marker string))
        ((class-p (node-before marker) "comment")
         (insert-nodes (end-pos (node-before marker)) string))
        (t
         (let ((node (make-atom-node "symbol" string)))
           (insert-nodes marker node)
           (setf (pos marker) (end-pos node))))))

(defmethod insert-text-aux
    ((buffer sexp-editing-mode) text-node parent)
  (if (symbol-node-p parent)
      text-node
      (make-atom-node "symbol" (text text-node))))

;;; DOM to Sexp parser

(defvar *form-node-table* nil
  "Hash table that map forms to node that generates them.")

(defun parse-prefix (string)
  "Parse prefix from STRING.
Return a list of wrapper functions and the rest of STRING.  The list
of wrapper functions can be applied to some Lisp object one-by-one and
reproduce the effect of parsed prefix."
  (let (wrappers (i 0))
    (iter (while (< i (length string)))
      (case (aref string i)
        ((#\')
         (incf i)
         (push (lambda (next) (list 'quote next)) wrappers))
        ((#\`)
         (incf i)
         (push (lambda (next) (list 'sb-int:quasiquote next)) wrappers))
        ((#\,)
         (incf i)
         (case (when (< i (length string))
                 (aref string i))
           ((#\@)
            (incf i)
            (push (lambda (next) (sb-int:unquote next 2)) wrappers))
           (t
            (push (lambda (next) (sb-int:unquote next 0)) wrappers))))
        ((#\#)
         (incf i)
         (case (when (< i (length string))
                 (aref string i))
           ((#\')
            (incf i)
            (push (lambda (next) (list 'function next)) wrappers))
           (t (decf i) (return))))
        (t (return))))
    (values wrappers (subseq string i))))

(defun ghost-symbol-p (node)
  "Test NODE is a symbol node with only prefix.

These symbol nodes do not correspond to Sexp symbols, instead act on
the following node."
  (when (symbol-node-p node)
    (bind (((:values wrappers rest)
            (parse-prefix (text-content node))))
      (when (zerop (length rest))
        wrappers))))

(defun node-to-sexp (node)
  "Parse DOM NODE as a Lisp object.
It also takes into account any prefix preceding NODE."
  (labels ((apply-wrappers (wrappers node)
             (iter (for w in wrappers)
               (setq node (funcall w node)))
             node)
           (process (node)
             (let ((sexp
                     (cond ((list-node-p node)
                            (mapcar #'process
                                    (remove-if (alex:disjoin
                                                #'text-node-p
                                                #'ghost-symbol-p
                                                #'new-line-node-p)
                                               (child-nodes node))))
                           ((equal "string" (attribute node "class"))
                            (text-content node))
                           ((symbol-node-p node)
                            (bind (((:values wrappers rest)
                                    (parse-prefix (text-content node))))
                              (apply-wrappers wrappers (read-from-string rest))))
                           ((new-line-node-p node) nil)
                           ((class-p node "object")
                            (or (attribute node 'presentation)
                                (error "Presentation attribute missing from ~a" node)))
                           (t (error "Unrecognized DOM node: ~a" node)))))
               (when *form-node-table*
                 (setf (gethash sexp *form-node-table*)
                       node))
               (if-let (wrappers (ghost-symbol-p (previous-sibling node)))
                 (apply-wrappers wrappers sexp)
                 sexp))))
    (process node)))

(defun current-package (&optional (marker-or-pos (focus)))
  "Return the package in the context of MARKER-OR-POS.
This is determined by searching for a `in-package' top-level form
before MARKER-OR-POS."
  (with-marker (marker marker-or-pos)
    (or (find-package
         (handler-case
             (iter (beginning-of-defun marker)
               (for node = (node-after marker))
               (when (and (list-node-p node)
                          (eql 'in-package (compute-symbol (first-child node))))
                 (let ((p (npos-right-until (first-child node) #'sexp-node-p)))
                   (when-let (s (compute-symbol p))
                     (return (symbol-name s))))))
           (top-of-subtree ())))
        (find-package "NEOMACS"))))

;;; Compiler notes

(defvar *compilation-buffer* nil
  "The buffer for outputting compilation notes.")

(defvar *compilation-document-root* nil
  "Document root of the buffer being compiled.

Used for resolving source-path to DOM node.")

(defvar *compilation-single-form* nil
  "If t, the first element in source-path should always be 0 and is ignored.")

(defun sexp-nth-child (node n)
  (nth n (remove-if
          (lambda (node)
            (or (not (sexp-node-p node))
                (ghost-symbol-p node)))
          (child-nodes node))))

(defun sexp-child-number (node)
  (position node (remove-if
                  (lambda (node)
                    (or (not (sexp-node-p node))
                        (ghost-symbol-p node)))
                  (child-nodes (parent node)))))

(defun find-node-for-form-path (root path)
  (iter (with node = root)
    (for i in path)
    (when-let (child (sexp-nth-child node i))
      (setq node child))
    (finally (return node))))

(defun find-node-for-compiler-note (context)
  (or
   (when-let (form (sb-c::compiler-error-context-original-form
                    context))
     (gethash form *form-node-table*))
   (when-let (path (reverse (sb-c::compiler-error-context-original-source-path context)))
     (when *compilation-single-form*
       (unless (eql (car path) 0)
         (warn "Compiling single form but source path ~a does not start with 0." path))
       (pop path))
     (find-node-for-form-path
      *compilation-document-root* path))))

(defun handle-notification-condition (condition)
  (let (id)
    (with-current-buffer *compilation-buffer*
      (let ((*inhibit-read-only* t)
            (node (make-element
                   "p" :children
                   (list (princ-to-string condition)))))
        (insert-nodes (focus) node)
        (setq id (attribute node "neomacs-identifier"))))

    (when-let* ((context (sb-c::find-error-context nil))
                (node (find-node-for-compiler-note context)))
      (setf (attribute node "compiler-note-severity")
            (typecase condition
              (sb-ext:compiler-note "note")
              (sb-c:compiler-error  "error")
              (error                "error")
              (style-warning        "style-warning")
              (warning              "warning")
              (t "error")))
      (setf (attribute node "compiler-note-id") id))))

(defmacro with-collecting-notes (() &body body)
  `(let ((*form-node-table* (make-hash-table))
         (*compilation-buffer*
           (get-buffer-create "*compilation*"
                              :modes '(read-only-mode))))
     (clear-compiler-notes)
     (with-current-buffer *compilation-buffer*
       (let ((*inhibit-read-only* t))
         (erase-buffer)))
     (handler-bind
         ((sb-c:fatal-compiler-error #'handle-notification-condition)
          (sb-c:compiler-error #'handle-notification-condition)
          (sb-ext:compiler-note #'handle-notification-condition)
          (error #'handle-notification-condition)
          (warning #'handle-notification-condition))
       ,@body)))

(defun clear-compiler-notes ()
  (do-elements
      (lambda (node)
        (when (attribute node "compiler-note-severity")
          (setf (attribute node "compiler-note-severity")
                nil)))
    (document-root (current-buffer))))

(defun goto-compiler-note (id)
  (when-let (buffer (get-buffer "*compilation*"))
    (with-current-buffer buffer
      (do-elements
          (lambda (node)
            (when (equal (attribute node "neomacs-identifier")
                         id)
              (setf (pos (focus)) node)
              (return-from goto-compiler-note)))
        (document-root buffer)))))

(define-command previous-compiler-note (&optional (marker (focus)))
  "Move to next compiler note."
  (if-let (pos
           (npos-prev-until
            (pos marker)
            (lambda (node)
              (and (element-p node)
                   (attribute node "compiler-note-id")))))
    (progn
      (setf (pos marker) pos)
      (goto-compiler-note (attribute pos "compiler-note-id")))
    (error "No previous compiler note")))

(define-command next-compiler-note (&optional (marker (focus)))
  "Move to previous compiler note."
  (if-let (pos
           (npos-next-until
            (pos marker)
            (lambda (node)
              (and (element-p node)
                   (attribute node "compiler-note-id")))))
    (progn
      (setf (pos marker) pos)
      (goto-compiler-note (attribute pos "compiler-note-id")))
    (error "No next compiler note")))

;;; Eval/compile commands

(define-command eval-defun :mode lisp-mode ()
  "Evaluate the surrounding top-level form.

Echo the result."
  (with-marker (marker (focus))
    (beginning-of-defun marker)
    (let* ((node (pos marker))
           (*package* (current-package marker))
           (result (eval (node-to-sexp node))))
      (message "=> ~a" result))))

(define-command compile-defun :mode lisp-mode ()
  "Compile the surrounding top-level form.

Highlights compiler notes."
  (with-marker (marker (focus))
    (beginning-of-defun marker)
    (let ((cookie (uuid:format-as-urn nil (uuid:make-v4-uuid)))
          (node (pos marker)))
      (setf (attribute node 'compile-cookie) cookie)
      (with-collecting-notes ()
        (with-compilation-unit
            (:source-plist
             (list :neomacs-buffer (name (current-buffer))
                   :neomacs-compile-cookie cookie))
          (let* ((*package* (current-package marker))
                 (*compilation-single-form* t)
                 (*compilation-document-root* node)
                 (input-file
                   (make-pathname
                    :name cookie :type "lisp"
                    :defaults (uiop:temporary-directory)))
                 output-file)
            (with-open-file (s input-file
                               :direction :output
                               :if-exists :supersede)
              (write-dom-aux (current-buffer) node s))
            (unwind-protect
                 (load (setq output-file (compile-file input-file)))
              (uiop:delete-file-if-exists input-file)
              (uiop:delete-file-if-exists output-file))
            (unless (frame-root *compilation-buffer*)
              (when (first-child (document-root *compilation-buffer*))
                (display-buffer-right *compilation-buffer*)))))))))

(defun last-expression (pos)
  (setq pos (resolve-marker pos))
  (when (and (end-pos-p pos)
             (symbol-node-p (end-pos-node pos)))
    (return-from last-expression (end-pos-node pos)))
  (iter
    (unless pos (error 'beginning-of-subtree))
    (for node = (node-before pos))
    (until (sexp-node-p node))
    (setq pos (npos-prev pos)))
  (node-before pos))

(define-command eval-last-expression :mode lisp-mode
  (&optional (marker (focus)))
  "Evaluate the last expression and echo the result."
  (let* ((*package* (current-package marker))
         (result (eval (node-to-sexp (last-expression (pos marker))))))
    (message "=> ~a" result)))

(define-command eval-print-last-expression :mode lisp-mode
  (&optional (marker (focus)))
  "Evaluate the last expression and insert result into current buffer."
  (let* ((*package* (current-package marker))
         (node (last-expression marker))
         (pos (pos-right node))
         (last-line (make-new-line-node)))
    (if (new-line-node-p (node-after pos))
        (setf pos (pos-right pos))
        (insert-nodes pos (make-new-line-node)))
    (insert-nodes pos (print-dom (eval (node-to-sexp node)))
                  last-line)
    (setf (pos marker) (pos-right last-line))))

(define-command lisp-compile-file :mode lisp-mode ()
  "Compile current file.

Highlight compiler notes."
  (when (read-yes-or-no "Save file? ")
    (save-buffer))
  (let ((*compilation-document-root*
          (document-root (current-buffer))))
    (with-collecting-notes ()
      (iter (with i = 0)
        (for c in (child-nodes *compilation-document-root*))
        (when (sexp-node-p c)
          (setf (attribute c 'tlf-number) i)
          (incf i)))
      (load (compile-file (file-path (current-buffer)))))))

;;; Xref

(defparameter *definition-types*
  '(:variable :constant :type :symbol-macro :macro
    :compiler-macro :function :generic-function :method
    :setf-expander :structure :condition :class
    :method-combination :package :transform :optimizer
    :vop :source-transform :ir1-convert :declaration
    :alien-type)
  "SB-INTROSPECT definition types.")

(defun find-node-for-source
    (pathname tlf-number form-number plist)
  (when-let (buf (or (get-buffer (getf plist :neomacs-buffer))
                     (find-file-buffer pathname)))
    (let* ((candidate-nodes
             (if-let (c (getf plist :neomacs-compile-cookie))
               (remove-if-not
                (lambda (n)
                  (equal c (attribute n 'compile-cookie)))
                (child-nodes (document-root (current-buffer))))
               (remove-if-not #'sexp-node-p
                              (document-root (current-buffer)))))
           (tlf (or (find tlf-number candidate-nodes
                          :key (alex:rcurry #'attribute 'tlf-number))
                    (nth tlf-number candidate-nodes)))
           (translation
             (sb-di::form-number-translations
              (node-to-sexp tlf) 0))
           (form-path
             (if (< form-number (length translation))
                 (reverse
                  (cdr (aref translation form-number)))
                 (progn
                   (message "Inconsistent form-number translation")
                   nil))))
      (pop form-path)
      (find-node-for-form-path tlf form-path))))

(defun visit-source (pathname tlf-number form-number plist)
  (with-current-buffer
      (or (get-buffer (getf plist :neomacs-buffer))
          (when pathname (find-file pathname))
          (error "Unknown source location"))
    (setf (pos (focus))
          (find-node-for-source
           pathname tlf-number form-number plist))))

(defun visit-definition (definition)
  "Switch to a buffer displaying DEFINITION.

DEFINITION should be a `sb-introspect:definition-source'."
  (visit-source
   (sb-introspect:definition-source-pathname definition)
   (car (sb-introspect:definition-source-form-path definition))
   (sb-introspect:definition-source-form-number definition)
   (sb-introspect:definition-source-plist definition)))

(define-mode xref-list-mode (list-mode)
  ((for-symbol :initform (alex:required-argument :symbol)
               :initarg :symbol)))

(define-keys xref-list-mode
  "q" 'quit-buffer
  "enter" 'xref-list-goto-definition)

(defmethod generate-rows ((buffer xref-list-mode))
  (let ((*print-case* :downcase)
        (symbol (for-symbol buffer)))
    (iter (for (type def) in (find-definitions symbol))
      (insert-nodes
       (focus)
       (lret ((el (make-element
                   "tr"
                   :children
                   (list (make-element
                          "td" :children
                          (list (prin1-to-string type)))
                         (make-element
                          "td" :children
                          (list (print-arglist
                                 (sb-introspect::definition-source-description def)
                                 (symbol-package symbol))))
                         (make-element
                          "td" :children
                          (list (princ-to-string
                                 (sb-introspect::definition-source-pathname def))))))))
         (setf (attribute el 'definition) def))))))

(defun find-definitions (symbol)
  "Find all definitions for SYMBOL.

Return a list with items of the form `(definition-type
sb-introspect:definition-source)'."
  (iter (for type in *definition-types*)
    (appending
     (mapcar (alex:curry #'list type)
             (sb-introspect:find-definition-sources-by-name
              symbol type)))))

(define-command xref-list-goto-definition
  :mode xref-list-mode ()
  (or (when-let (row (focused-row))
        (visit-definition
         (attribute row 'definition))
        (quit-buffer)
        t)
      (message "No xref item under focus")))

(define-command goto-definition
  :mode lisp-mode ()
  "Goto definition of symbol under focus."
  (when-let (node (symbol-around (focus)))
    (let ((name (nth-value 1 (parse-prefix (text-content node)))))
      (when (> (length name) 0)
        (when-let (symbol (find-symbol (string-upcase name)
                                       (current-package)))
          (let ((definitions (find-definitions symbol)))
            (if (= (length definitions) 1)
                (visit-definition (cadr (car definitions)))
                (focus-buffer
                 (display-buffer-right
                  (with-current-buffer
                      (make-buffer
                       "*xref*" :modes '(xref-list-mode)
                       :symbol symbol)
                    (revert-buffer)
                    (current-buffer)))))))))))

;;; Auto-completion

(defun atom-around (pos)
  (let ((parent (node-containing pos))
        (before (node-before pos))
        (after (node-after pos)))
    (cond ((atom-node-p parent) parent)
          ((atom-node-p before) before)
          ((atom-node-p after) after))))

(defun symbol-around (pos)
  (when-let (node (atom-around pos))
    (when (equal "symbol" (attribute node "class"))
      node)))

(defun comment-around (pos)
  (when-let (node (atom-around pos))
    (when (equal "comment" (attribute node "class"))
      node)))

(defmethod compute-completion ((buffer lisp-mode) pos)
  (when-let (node (symbol-around pos))
    (let* ((package (current-package pos))
           (text (text-content node))
           (name (nth-value 1 (parse-prefix text)))
           (swank::*buffer-package* package)
           (completions (car (swank:fuzzy-completions
                              name package))))
      (values
       (range (text-pos (first-child node) (- (length text) (length name)))
              (pos-down-last node))
       (iter (for c in completions)
         (collect (list (car c) (remove #\- (lastcar c)))))))))

;;; Parser

(defun read-string (stream c)
  (declare (ignore c))
  (append-child
   *dom-output*
   (make-atom-node
    "string"
    (iter (for c = (read-char stream))
      (until (eql c #\"))
      (if (eql c #\\)
          (collect (read-char stream) result-type string)
          (collect c result-type string))))))

(defun read-line-comment (stream c)
  (declare (ignore c))
  (let ((n 1))
    (iter (for c = (peek-char nil stream nil nil t))
      (while (eql c #\;))
      (incf n)
      (read-char stream))
    (bind (((:values line eof-p) (read-line stream))
           (node (make-atom-node "comment" line)))
      (setf (attribute node "comment-level") (prin1-to-string n))
      (append-child *dom-output* node)
      (unless eof-p (unread-char #\Newline stream)))))

(defvar *lisp-syntax-table*
  (lret ((table (make-syntax-table)))
    (set-syntax-range table 33 127 'symbol)
    (setf (get-syntax-table #\( table) (make-read-delimited #\)))
    (setf (get-syntax-table #\) table) nil)
    (setf (get-syntax-table #\  table) 'read-ignore)
    (setf (get-syntax-table #\Newline table) 'read-newline)
    (setf (get-syntax-table #\Page table) 'read-newline)
    (setf (get-syntax-table #\Tab table) 'read-ignore)
    (setf (get-syntax-table #\" table) 'read-string)
    (setf (get-syntax-table #\\ table) 'single-escape)
    (setf (get-syntax-table #\; table) 'read-line-comment)))

(defmethod read-dom-aux ((buffer lisp-mode) stream)
  (let ((*syntax-table* *lisp-syntax-table*))
    (read-dispatch *syntax-table* stream)))

;;; Pretty printer

(defun symbol-indentation (symbol)
  (case symbol
    ((lambda) 1)
    ((block catch return-from throw eval-when
            multiple-value-call multiple-value-prog1
            unwind-protect)
     1)
    ((locally progn) 0)
    ((progv) 1)
    ((flet labels macrolet)
     '((&whole 4 &rest (&whole 1 4 4 &rest 2)) &rest 2))
    ((let let* symbol-macrolet dx-let)
     '((&whole 4 &rest (&whole 1 1 2)) &rest 2))
    ((case ccase ecase)
     '(4 &rest (&whole 2 &rest 1)))
    ((handler-case handler-bind) 1)
    (t (when-let (mf (macro-function symbol))
         (or (sb-pretty::macro-indentation mf)
             (when (sera:string-prefix-p "DEF" (symbol-name symbol))
               1))))))

(defun normalize-indent-spec (indent-spec)
  (when indent-spec
    (when (numberp indent-spec)
      (setq indent-spec
            (nconc (make-list indent-spec :initial-element 4)
                   (list '&rest 2))))
    (push nil indent-spec)
    indent-spec))

(defun pprint-form (list-node stream indent-spec)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (if indent-spec
        (iter
          (with prev = nil)
          (for c in (child-nodes list-node))
          (unless (or (first-iteration-p)
                      (new-line-node-p prev))
            (write-char #\space stream))
          (if (sexp-node-p c)
              (let* ((this-indent-spec
                       (if (eql (car indent-spec) '&rest)
                           (cadr indent-spec)
                           (pop indent-spec)))
                     (next-indent-spec
                       (if (eql (car indent-spec) '&rest)
                           (cadr indent-spec)
                           (car indent-spec)))
                     (indent-number
                       (or (when (numberp next-indent-spec)
                             next-indent-spec)
                           (getf next-indent-spec '&whole)
                           1)))
                (pprint-indent :block (1- indent-number) stream)
                (if (and (listp this-indent-spec) (list-node-p c))
                    (progn
                      (setq this-indent-spec (copy-tree this-indent-spec))
                      (remf this-indent-spec '&whole)
                      (pprint-form c stream this-indent-spec))
                    (write c :stream stream)))
              (write c :stream stream))
          (setq prev c))
        ;; Print function call form
        (iter
          (with prev = nil)
          (with i = 0)
          (for c in (child-nodes list-node))
          (unless (or (first-iteration-p)
                      (new-line-node-p prev)
                      (ghost-symbol-p prev))
            (write-char #\space stream))
          (when (and (sexp-node-p c) (<= i 1))
            (pprint-indent :current 0 stream))
          (write c :stream stream)
          (when (sexp-node-p c)
            (incf i))
          (setq prev c)))))

(defvar *lisp-pprint-dispatch*
  (lret ((*print-pprint-dispatch*
          (copy-pprint-dispatch *print-pprint-dispatch*)))
    (set-pprint-dispatch
     'element
     (lambda (stream self &rest noise)
       (declare (ignore noise))
       (cond ((list-node-p self)
              (pprint-form self stream
                           (normalize-indent-spec
                            (symbol-indentation
                             (compute-symbol (first-child self))))))
             ((new-line-node-p self)
              (pprint-newline :mandatory stream))
             ((symbol-node-p self)
              (write-string (text-content self) stream))
             ((equal (attribute self "class") "string")
              (write
               (with-output-to-string (s)
                 (iter (for c in (child-nodes self))
                   (write c :stream s)))
               :stream stream))
             ((equal (attribute self "class") "comment")
              (dotimes (_ (parse-number:parse-number (attribute self "comment-level")))
                (write-char #\; stream))
              (write-char #\  stream)
              (iter (for c in (child-nodes self))
                (write c :stream stream)))
             (t (error "TODO")))))

    (set-pprint-dispatch
     'text-node
     (lambda (stream self &rest noise)
       (declare (ignore noise))
       (write-string (text self) stream)))))

(defmethod write-dom-aux ((buffer lisp-mode) node stream)
  (let ((*print-pprint-dispatch* *lisp-pprint-dispatch*)
        (*package* (find-package "NEOMACS"))
        (*print-pretty* t))
    (prin1 node stream)))

;;; Style

(defstyle sexp-node `(((:append ":not(:last-child):not(.focus-tail)")
                       :margin-right "0.4em")
                      :display "inline-block"
                      :vertical-align "top"
                      :position "relative"
                      :white-space "pre-wrap"))
(defstyle list-node
    `(((:append "::before")
       :content "("
       :margin-left "-0.4em")
      ((:append "::after")
       :content ")"
       :vertical-align "bottom")
      ((:append ":not(:last-child)")
       :margin-right "0.4em")
      ((:append ".focus-tail::after")
       :content ")"
       :width "auto"
       :inherit selection)
      ((:append ".focus::before")
       :inherit selection)
      :inherit sexp-node
      :padding-left "0.4em"))

(defstyle string-node `(((:append "::before") :content "\"")
                        ((:append "::after") :content "\"")
                        ((:append ":not(:last-child)")
                         :margin-right "0.4em")
                        ((:append ".focus::before")
                         :inherit selection)
                        ((:append ".focus-tail::after")
                         :content "\""
                         :width "auto"
                         :inherit selection)
                        :inherit (sexp-node string)))
(defstyle symbol-node `(:inherit sexp-node))
(defstyle object-node `(:inherit symbol-node
                        :text-decoration "underline"))
(defstyle empty-symbol-node `(((:append "::after")
                               :content "_")
                              ((:append ":not(:last-child)")
                               :margin-right "0.4em")
                              ((:append ".focus-tail::after")
                               :content "_"
                               :width "auto"
                               :inherit selection)))
(defstyle comment-node `(((:append "::after") :content "⁣")
                         :display "inline-block"
                         :white-space "pre-wrap"))
(defstyle comment-node-1 `(:position "sticky"
                           :left "20em"
                           :border-left "0.3rem solid #a997a0"
                           :padding-left "0.3rem"
                           :inherit comment))
(defstyle comment-node-2 `(:border-left "0.3rem solid #a997a0"
                           :padding-left "0.3rem"
                           :inherit comment))
(defstyle comment-node-3 `(((:append "::before")
                            :content ""
                            :display "list-item"
                            :position "absolute")
                           :font-size "1.2em"
                           :inherit comment))
(defstyle comment-node-4 `(((:append "::before")
                            :content ""
                            :display "list-item"
                            :position "absolute")
                           :font-size "1.1em"
                           :inherit comment))

(defstyle compiler-note
    `(:outline "solid rgba(200,200,200,1.0)"))
(defstyle compiler-style-warning
    `(:outline "solid rgba(150,150,255,1.0)"))
(defstyle compiler-warning
    `(:outline "solid rgba(255,150,0,1.0)"))
(defstyle compiler-error
    `(:outline "solid rgba(255,75,0,1.0)"))

(defstyle lisp-mode
    `((".symbol" :inherit symbol-node)
      (".symbol:empty" :inherit empty-symbol-node)
      (".string" :inherit string-node)
      (".object" :inherit object-node)
      (".symbol[symbol-type=\"macro\"][operator]" :inherit macro)
      (".symbol[symbol-type=\"keyword\"]" :inherit keyword)
      (".symbol[symbol-type=\"special-operator\"][operator]" :inherit special-operator)
      (".list" :inherit list-node)
      (".comment" :inherit comment-node)
      (".comment[comment-level=\"1\"]" :inherit comment-node-1)
      (".comment[comment-level=\"2\"]" :inherit comment-node-2)
      (".comment[comment-level=\"3\"]" :inherit comment-node-3)
      (".comment[comment-level=\"4\"]" :inherit comment-node-4)
      ("[compiler-note-severity=\"note\"]"
       :inherit compiler-note)
      ("[compiler-note-severity=\"style-warning\"]"
       :inherit compiler-style-warning)
      ("[compiler-note-severity=\"warning\"]"
       :inherit compiler-warning)
      ("[compiler-note-severity=\"error\"]"
       :inherit compiler-error)))

;;; Default hooks
#+nil (add-mode-hook 'lisp-mode 'undo-mode)
#+nil (add-mode-hook 'lisp-mode 'auto-completion-mode)
