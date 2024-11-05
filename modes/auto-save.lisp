(in-package #:neomacs)

(defun make-auto-save-path (file-path)
  (make-pathname
   :name (str:concat "#" (pathname-name file-path))
   :type (str:concat (pathname-type file-path) "#")
   :defaults file-path))

(define-mode auto-save-mode (file-mode)
  ((key-count :initform 0 :type (integer 0))
   (auto-save-key-count :default 300 :type (integer 1)))
  (:toggler t))

(defmethod enable-aux ((mode-name (eql 'auto-save-mode)))
  (let* ((buffer (current-buffer))
         (path (make-auto-save-path (file-path buffer))))
    (when (and (osicat:file-exists-p (file-path buffer))
               (osicat:file-exists-p path)
               (<
                (osicat-posix:stat-mtime
                 (osicat-posix:stat (file-path buffer)))
                (osicat-posix:stat-mtime
                 (osicat-posix:stat path))))
      (message "Auto-save file is newer than ~a. Consider M-x recover-this-file."
               (name (current-buffer))))))

(defgeneric auto-save-aux (buffer)
  (:method ((buffer buffer))
    (not-supported buffer 'auto-save)))

(defmethod auto-save-aux ((buffer auto-save-mode))
  (with-open-file (s (make-auto-save-path (file-path buffer))
                     :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (c (child-nodes (document-root buffer)))
        (write-dom-aux buffer c s))
      nil)))

(defmethod save-buffer-aux :after ((buffer auto-save-mode))
  (uiop:delete-file-if-exists
   (make-auto-save-path (file-path buffer))))

(defmethod check-read-only :after
    ((buffer auto-save-mode) (pos t))
  (if (< (key-count buffer) (auto-save-key-count buffer))
      (incf (key-count buffer))
      (progn
        (setf (key-count buffer) 0)
        (auto-save-aux buffer))))

(define-command auto-save-all ()
  "Save all buffers that need to be auto-saved."
  (let ((saved-count 0))
    (dolist (buffer (alex:hash-table-values *buffer-table*))
      (when (and (typep buffer 'auto-save-mode)
                 (modified buffer))
        (with-demoted-errors
            (format nil "Error auto-saving ~a: " buffer)
          (auto-save-aux buffer)
          (incf saved-count))))
    (message "~a buffer~p auto-saved" saved-count saved-count)))

(define-command recover-this-file
  :mode file-mode (&optional (buffer (current-buffer)))
  "Recover buffer content from its auto-save file."
  (let ((save-path (make-auto-save-path (file-path buffer))))
    (unless (uiop:file-exists-p save-path)
      (user-error "No auto-save file"))
    (erase-buffer)
    (apply #'insert-nodes (end-pos (document-root buffer))
           (read-from-file save-path))
    (setf (restriction buffer) (document-root buffer)
          (pos (focus buffer)) (pos-down (document-root buffer)))))

(pushnew 'auto-save-mode (hooks 'file-mode))
