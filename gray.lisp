(defpackage neomacs/gray
  (:use cl sb-gray)
  (:export neomacs-output-stream
           make-stream-data
           make-auto-flush-thread))

(in-package #:neomacs/gray)

;;; Auto-flush stream
;; Adapted from swank-gray. We have to modify it so that it doesn't
;; deadlock. Specifically, swank-gray calls output-fn inside
;; with-stream-data, and our output-fn grabs lock on buffers. We move
;; calls to output-fn out.

(defstruct stream-data
  (output-fn)
  (buffer (make-string 64000))
  (fill-pointer 0)
  (column 0)
  (lock (bt2:make-recursive-lock :name "buffer write lock"))
  (flush-mailbox)
  (flush-scheduled))

(defclass neomacs-output-stream (fundamental-character-output-stream)
  ((data :initform (make-stream-data)
         :initarg :data
         :accessor data)))

(defmacro with-stream-data (data &body body)
  `(with-accessors ((lock stream-data-lock)
                    (buffer stream-data-buffer)
                    (fill-pointer stream-data-fill-pointer)
                    (column stream-data-column)
                    (flush-mailbox stream-data-flush-mailbox)
                    (flush-scheduled stream-data-flush-scheduled))
       ,data
     (bt2:with-recursive-lock-held (lock) ,@body)))

(defmacro with-neomacs-output-stream (stream &body body)
  `(let ((data (data ,stream)))
     (with-stream-data data ,@body)))

(defun maybe-schedule-flush (data)
  (with-stream-data data
    (when flush-mailbox
      (or flush-scheduled
          (progn
            (setf flush-scheduled t)
            (sb-concurrency:send-message flush-mailbox t)
            t)))))

(defmethod stream-write-char ((stream neomacs-output-stream) char)
  (with-neomacs-output-stream stream
    (setf (schar buffer fill-pointer) char)
    (incf fill-pointer)
    (incf column)
    (when (char= #\newline char)
      (setf column 0))
    (if (= fill-pointer (length buffer))
        (%stream-finish-output data)
        (maybe-schedule-flush data)))
  char)

(defmethod stream-write-string ((stream neomacs-output-stream) string
                                &optional start end)
  (let (output)
    (with-neomacs-output-stream stream
      (let* ((start (or start 0))
             (end (or end (length string)))
             (len (length buffer))
             (count (- end start))
             (free (- len fill-pointer)))
        (when (>= count free)
          (%stream-finish-output data))
        (cond ((< count len)
               (replace buffer string :start1 fill-pointer
                                      :start2 start :end2 end)
               (incf fill-pointer count)
               (maybe-schedule-flush data))
              (t
               (setq output (subseq string start end))))
        (let ((last-newline (position #\newline string :from-end t
                                                       :start start :end end)))
          (setf column (if last-newline
                           (- end last-newline 1)
                           (+ column count))))))
    (when output
      (funcall (stream-data-output-fn (data stream))
               output)))
  string)

(defmethod stream-line-column ((stream neomacs-output-stream))
  (with-neomacs-output-stream stream column))

(defun reset-stream-line-column (stream)
  (with-neomacs-output-stream stream (setf column 0)))

(defun %stream-finish-output (data)
  (let (output)
    (with-stream-data data
      (unless (zerop fill-pointer)
        (setq output (subseq buffer 0 fill-pointer))
        (setf fill-pointer 0))
      (setf flush-scheduled nil))
    (when output
      (funcall (stream-data-output-fn data)
               output)))
  nil)

(defmethod stream-force-output ((stream neomacs-output-stream))
  (stream-finish-output stream))

(defmethod stream-finish-output ((stream neomacs-output-stream))
  (with-neomacs-output-stream stream
    (unless (maybe-schedule-flush data)
      (%stream-finish-output data))))

(defmethod stream-fresh-line ((stream neomacs-output-stream))
  (with-neomacs-output-stream stream
    (cond ((zerop column) nil)
          (t (terpri stream) t))))

(defmethod stream-file-position ((stream neomacs-output-stream) &optional position)
  (declare (ignore position))
  nil)

(defun make-auto-flush-thread (stream)
  (check-type stream neomacs-output-stream)
  (let ((mailbox (sb-concurrency:make-mailbox)))
    (setf (stream-data-flush-mailbox (data stream)) mailbox)
    (bt:make-thread
     (lambda ()
       (loop
         (when (not (open-stream-p stream))
           (return nil))
         (%stream-finish-output (data stream))
         (sb-concurrency:receive-message mailbox)
         (sleep 0.005)))
     :name "*auto-flush*")))
