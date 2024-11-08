(in-package #:neomacs)

(sera:export-always
    '(*dom-output* read-dom-aux read-dom read-from-file
      read-dispatch append-text make-read-delimited
      write-dom-aux))

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

(defvar *dom-output* nil)

(defun call-with-dom-output (body)
  (if *dom-output* (funcall body)
      (let ((*dom-output* (make-instance 'element :tag-name "div")))
        (funcall body)
        (child-nodes *dom-output*))))

(defgeneric read-dom-aux (buffer stream))

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

(defun read-dom (stream &optional recursive-p)
  (if recursive-p (read-dom-aux (current-buffer) stream)
      (call-with-dom-output
       (lambda () (read-dom-aux (current-buffer) stream)))))

(defun read-from-file (file)
  (with-open-file (s file :direction :input
                          :if-does-not-exist nil)
    (when s
      (call-with-dom-output
       (lambda ()
         (handler-case
             (loop (read-dom s t))
           (end-of-file ())))))))

(defun append-text (parent string)
  (if (text-node-p (last-child parent))
      (setf (text (last-child parent))
            (str:concat (text (last-child parent)) string))
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

;;; Pretty printer interface

(defgeneric write-dom-aux (buffer node stream))
