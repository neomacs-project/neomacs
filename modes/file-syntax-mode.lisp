(in-package #:neomacs)

;;; Syntax table

(defun get-syntax-table (char table)
  (gethash char table))

(defun (setf get-syntax-table) (new-val char table)
  (setf (gethash char table) new-val))

(defun set-syntax-range (table beg end syntax-class)
  (setq beg (if (characterp beg) (char-code beg) beg))
  (setq end (if (characterp end) (char-code end) end))
  (iter (for i from beg to end)
    (setf (gethash (code-char i) table)
          syntax-class)))

(defun make-syntax-table (&rest bindings)
  (lret ((table (make-hash-table)))
    (iter (for (k v) on bindings by #'cddr)
      (setf (get-syntax-table k table) v))))

(defvar *syntax-table* nil)

;;; Read to DOM

(defun read-dom (stream &optional recursive-p)
  (if recursive-p (read-dispatch *syntax-table* stream)
      (call-with-dom-output
       (lambda () (read-dispatch *syntax-table* stream)))))

(defun read-dispatch (table stream)
  (let* ((c (peek-char nil stream))
         (syntax-class (get-syntax-table c table)))
    (etypecase syntax-class
      (function
       (funcall syntax-class stream (read-char stream)))
      (hash-table
       (read-char stream)
       (read-dispatch syntax-class stream))
      (null
       (if-let (default (get-syntax-table t table))
         (funcall default stream c)
         (error "Invalid character ~a." c)))
      (symbol
       (if (fboundp syntax-class)
           (funcall syntax-class stream (read-char stream))
           (read-constituent
            stream syntax-class
            (or (get-syntax-table syntax-class table)
                (lambda (result)
                  (append-child
                   *dom-output*
                   (make-atom-node
                    (string-downcase (symbol-name syntax-class))
                    result))))))))))

(defun read-from-file (file)
  (with-open-file (s file :direction :input)
    (call-with-dom-output
     (lambda ()
       (handler-case
           (loop (read-dom s t))
         (end-of-file ()))))))

(defun append-text (parent string)
  (if (text-node-p (last-child parent))
      (setf (text (last-child parent))
            (append (text (last-child parent)) string))
      (append-child parent (make-instance 'text-node :text string))))

(defnclo read-delimited (delimiter) (stream c)
  (declare (ignore c))
  (append-child
   *dom-output*
   (lret ((*dom-output* (make-list-node nil)))
     (iter (for c = (peek-char nil stream))
       (until (when (eql c delimiter)
                (read-char stream)
                t))
       (read-dom stream t)))))

(defun read-constituent (stream syntax-class filter)
  (iter (for c = (read-char stream nil nil t))
    (when (eql (get-syntax-table c *syntax-table*) 'single-escape)
      (collect c result-type string into result)
      (collect (read-char stream nil nil t) result-type string into result)
      (setq c (read-char stream nil nil t)))
    (while c)
    (until
     (unless (eql (get-syntax-table c *syntax-table*) syntax-class)
       (unread-char c stream)
       t))
    (collect c result-type string into result)
    (finally
     (funcall filter result))))

(defun read-newline (stream c)
  (declare (ignore stream c))
  (append-child *dom-output* (make-new-line-node)))

(defun read-ignore (stream c)
  (declare (ignore stream c)))

(defun text-filter (result)
  (append-text *dom-output* result))

(defun ignore-filter (result))

;;; File Syntax Mode

(define-class file-syntax-mode (file-mode)
  ((syntax-table))
  (:documentation "File mode for parsing file using a syntax table."))

(defmethod revert-buffer-aux ((buffer file-syntax-mode))
  (erase-buffer)
  (let ((doc-node (make-element "div" :class "doc"))
        (*syntax-table* (syntax-table (current-buffer))))
    (insert-nodes (end-pos (document-root buffer)) doc-node)
    (apply #'insert-nodes (end-pos doc-node)
           (read-from-file (file-path buffer)))
    (setf (restriction buffer) doc-node
          (pos (focus buffer)) (pos-down doc-node))))

(defmethod write-file ((buffer file-syntax-mode))
  (with-open-file (s (file-path buffer)
                     :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-pretty* t)
            (*print-pprint-dispatch* *lisp-pprint-dispatch*)
            (*package* (find-package "NEOMACS")))
        (dolist (c (child-nodes
                    (only-elt (get-elements-by-class-name
                               (document-root buffer)
                               "doc"))))
          (prin1 c s))
        nil))))
