(in-package :neomacs)

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
    (iter (for c = (peek-char t stream nil nil t))
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
    (setf (get-syntax-table #\Tab table) 'read-ignore)
    (setf (get-syntax-table #\" table) 'read-string)
    (setf (get-syntax-table #\\ table) 'single-escape)
    (setf (get-syntax-table #\; table) 'read-line-comment)))

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
                      (new-line-node-p prev))
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
              (write-string (atom-node-text self) stream))
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

;;; File mode

(define-class lisp-file-mode (lisp-mode file-syntax-mode)
  ((syntax-table :initform *lisp-syntax-table*))
  (:documentation "Lisp source files."))
