(in-package #:neomacs)

(sera:export-always
    '(get-syntax-table set-syntax-range make-syntax-table
      *syntax-table* *dom-output* read-dom-aux read-dom read-from-file
      read-dispatch append-text make-read-delimited read-delimited
      read-constituent read-text read-newline read-ignore
      write-dom-aux))

;;; Syntax table

(defun get-syntax-table (char table)
  "Get function bound to CHAR in TABLE."
  (gethash char table))

(defun (setf get-syntax-table) (new-val char table)
  (setf (gethash char table) new-val))

(defun set-syntax-range (table beg end symbol)
  "Bind characters betwen char-code BEG and END in TABLE to SYMBOL."
  (setq beg (if (characterp beg) (char-code beg) beg))
  (setq end (if (characterp end) (char-code end) end))
  (iter (for i from beg to end)
    (setf (gethash (code-char i) table)
          symbol)))

(defun make-syntax-table (&rest bindings)
  "Make a syntax table using BINDINGS."
  (lret ((table (make-hash-table)))
    (iter (for (k v) on bindings by #'cddr)
      (setf (get-syntax-table k table) v))))

(defvar *syntax-table* nil "The syntax table currently in effect.")

;;; Read to DOM

(defvar *dom-output* nil
  "A DOM node to accumulate result of parsers.")

(defun call-with-dom-output (body)
  (if *dom-output* (funcall body)
      (let ((*dom-output* (make-instance 'element :tag-name "div")))
        (funcall body)
        (child-nodes *dom-output*))))

(defgeneric read-dom-aux (buffer stream)
  (:documentation
   "Read and build DOM nodes from STREAM for BUFFER.

Resulting DOM nodes should be appended as children of
`*dom-output*'. Reading can stop at whatever boundary that makes
sense, i.e. multiple children can be built and appended."))

(defun read-dispatch (table stream)
  "Read and build DOM node from STREAM using syntax TABLE.

Resulting DOM nodes are appended as children of `*dom-output*'."
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
       (funcall syntax-class stream (read-char stream))))))

(defun read-dom (stream &optional recursive-p)
  "Read and build DOM nodes from STREAM using `*syntax-table*'.

If RECURSIVE-P is t, the call is expected to be made from within some
function that has been itself called from `read-dom', for example some
function that is bound in `*syntax-table*'.

If `*dom-output*' is bound, append the results as children of `*dom-output*'. Otherwise, return the results as a list of DOM nodes."
  (if recursive-p (read-dom-aux (current-buffer) stream)
      (call-with-dom-output
       (lambda () (read-dom-aux (current-buffer) stream)))))

(defun read-from-file (file)
  "Read and build DOM nodes from FILE using `*syntax-table*'.

Read continues until all FILE content is consumed.

If `*dom-output*' is bound, append the results as children of `*dom-output*'. Otherwise, return the results as a list of DOM nodes."
  (with-open-file (s file :direction :input
                          :if-does-not-exist nil)
    (when s
      (call-with-dom-output
       (lambda ()
         (handler-case
             (loop (read-dom s t))
           (end-of-file ())))))))

(defun append-text (parent string)
  "Append STRING as a text node to PARENT.

If PARENT already has a text node as `last-child', concat into the
text node instead."
  (if (text-node-p (last-child parent))
      (setf (text (last-child parent))
            (str:concat (text (last-child parent)) string))
      (append-child parent (make-instance 'text-node :text string))))

(defnclo read-delimited (delimiter) (stream c)
  "Read delimited list from STREAM, terminated by DELIMITER."
  (declare (ignore c))
  (append-child
   *dom-output*
   (lret ((*dom-output* (make-list-node nil)))
     (iter (for c = (peek-char nil stream))
       (until (when (eql c delimiter)
                (read-char stream)
                t))
       (read-dom stream t)))))

(defun read-constituent (stream symbol escape-chars)
  "Read consecutive characters from STREAM and return as string.

Read stops when a character not bound to SYMBOL in `*syntax-table*' is
encountered, with one exception: a character in the list ESCAPE-CHARS
makes the next character accepted unconditionally."
  (iter (for c = (read-char stream nil nil t))
    (when (member c escape-chars)
      (collect c result-type string)
      (collect (read-char stream nil nil t) result-type string)
      (setq c (read-char stream nil nil t)))
    (while c)
    (until
     (unless (eql (get-syntax-table c *syntax-table*) symbol)
       (unread-char c stream)
       t))
    (collect c result-type string)))

(defun read-newline (stream c)
  "Append a new line node (br element) to `*dom-output*'."
  (declare (ignore stream c))
  (append-child *dom-output* (make-new-line-node)))

(defun read-ignore (stream c)
  "Does nothing."
  (declare (ignore stream c)))

(defun read-text (stream c)
  "Read consecutive characters and append as text node to `*dom-output*'."
  (unread-char c stream)
  (append-text *dom-output*
               (read-constituent stream 'read-text nil)))

;;; Pretty printer interface

(defgeneric write-dom-aux (buffer node stream)
  (:documentation
   "Serialize DOM NODE to STREAM for BUFFER.

The result should be able to be read back with `read-dom-aux'."))
