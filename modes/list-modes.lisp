(in-package #:neomacs)

(define-class list-mode () ())

(defgeneric generate-rows (buffer))

(defgeneric focused-item (buffer))

(define-keymap list-mode ()
  "q" 'bury-buffer)

(defmethod read-only-p ((buffer list-mode)) t)

(defmethod revert-buffer-aux ((buffer list-mode))
  (erase-buffer)
  (let ((body-node (make-element "tbody")))
    (insert-nodes (end-pos (document-root buffer))
                  (make-element "table" :children (list body-node)))
    (setf (pos (focus)) (end-pos body-node)
          (restriction buffer) body-node)
    (generate-rows buffer)
    (setf (pos (focus)) (pos-down body-node))))

(define-class command-list-mode (list-mode) ())

(defun short-doc (command)
  (let ((doc (documentation command 'function)))
    (subseq doc 0 (position #\Newline doc))))

(defmethod generate-rows ((buffer command-list-mode))
  (iter (for c in *commands*)
    (for name = (string-downcase (symbol-name c)))
    (for doc = (short-doc c))
    (insert-nodes (focus)
                  (dom `(:tr
                         (:td ,name)
                         (:td ,@(when doc (list doc))))))))

(define-class buffer-list-mode (list-mode)
  ((show-hidden
    :initform nil :initarg :show-hidden
    :documentation "Whether to show hidden buffers, i.e. those with name started with a space.")))

(define-keymap buffer-list-mode ()
  "enter" 'buffer-list-switch-to-buffer
  "k" 'buffer-list-delete-buffer
  "d" 'buffer-list-delete-buffer
  "t" 'buffer-list-toggle-hidden)

(define-command buffer-list-switch-to-buffer ()
  (switch-to-buffer (focused-item (current-buffer))))

(define-command buffer-list-delete-buffer ()
  (delete-buffer (focused-item (current-buffer))))

(define-command buffer-list-toggle-hidden ()
  (setf (show-hidden (current-buffer))
        (not (show-hidden (current-buffer))))
  (revert-buffer))

(defmethod generate-rows ((buffer buffer-list-mode))
  (iter (for (name buf) in-hashtable *buffer-name-table*)
    (for modes =
         (sera:mapconcat (alex:compose #'string-downcase #'symbol-name)
                         (modes buf) " "))
    (unless (and (not (show-hidden buffer))
                 (sera:string-prefix-p " " name))
      (insert-nodes (focus)
                    (dom `((:tr :buffer ,(id buf))
                           (:td ,name)
                           (:td ,@(when (> (length modes) 0)
                                    (list modes)))))))))

(define-command list-commands ()
  (with-current-buffer
      (switch-to-buffer
       (get-buffer-create "*commands*" :modes '(command-list-mode)))
    (revert-buffer)))

(define-command list-buffers ()
  (with-current-buffer
      (switch-to-buffer
       (get-buffer-create "*buffers*" :modes '(buffer-list-mode)))
    (revert-buffer)))

(defun focused-row ()
  (pos-up-ensure (focus) (alex:rcurry #'tag-name-p "tr")))

(defmethod focused-item ((buffer buffer-list-mode))
  (or (when-let (row (focused-row))
        (gethash (parse-integer (attribute row "buffer")) *buffer-table*))
      (error "No focused buffer")))

(defstyle list-mode `(("table" :white-space "pre" :width "100%"
                               :border-collapse "collapse")
                      ("tbody:empty::after"
                       :content "<No Item>"
                       :display "inline")))

(define-class file-list-mode (list-mode)
  ((file-path
    :initform *default-pathname-defaults*
    :documentation "Path of the directory this buffer is visiting.

This should always be a directory pathname (with NIL name and type fields).")
   (header-p :initform t :initarg :header-p)))

(define-keymap file-list-mode ()
  "enter" 'file-list-find-file)

(defun file-size-readable (file-size &optional flavor space unit)
  (let ((power (if (or (null flavor) (eq flavor 'iec))
		   1024.0
		   1000.0))
	(prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y" "R" "Q")))
    (iter (while (and (>= file-size power) (cdr prefixes)))
      (setq file-size (/ file-size power)
	    prefixes (cdr prefixes)))
    (let* ((prefix (car prefixes))
           (prefixed-unit (if (eq flavor 'iec)
                              (str:concat
                               (if (equal prefix "k") "K" prefix)
                               (if (equal prefix "") "" "i")
                               (or unit "B"))
                              (str:concat prefix unit))))
      (if (and (< file-size 10)
               (>= (mod file-size 1.0) 0.05)
               (< (mod file-size 1.0) 0.95))
	  (format nil
                  "~1$~a~a"
	          file-size
                  (if (equal prefixed-unit "") "" (or space ""))
                  prefixed-unit)
	  (format nil
                  "~a~a~a"
	          (round file-size)
                  (if (equal prefixed-unit "") "" (or space ""))
                  prefixed-unit)))))

(defun file-date-readable (unix-time)
  (local-time:format-timestring
   nil (local-time:unix-to-timestamp unix-time)
   :format local-time:+asctime-format+))

(defmethod generate-rows ((buffer file-list-mode))
  (iter (for path in (uiop:subdirectories (file-path buffer)))
    (for stat = (osicat-posix:stat path))
    (insert-nodes (focus)
                  (dom `(:tr
                         ((:td :class "directory")
                          ,(lastcar (pathname-directory path)))
                         (:td)
                         (:td ,(osicat-posix:getpwuid
                                (osicat-posix:stat-uid stat)))
                         (:td ,(file-date-readable
                                (osicat-posix:stat-mtime stat)))))))
  (iter (for path in (uiop:directory-files (file-path buffer)))
    (for stat = (osicat-posix:stat path))
    (insert-nodes (focus)
                  (dom `(:tr
                         (:td ,(file-namestring path))
                         (:td ,(file-size-readable (osicat-posix:stat-size stat)))
                         (:td ,(osicat-posix:getpwuid
                                (osicat-posix:stat-uid stat)))
                         (:td ,(file-date-readable
                                (osicat-posix:stat-mtime stat))))))))

(defmethod revert-buffer-aux ((buffer file-list-mode))
  (call-next-method)
  (when (header-p buffer)
    (insert-nodes
     (pos-down (document-root buffer))
     (make-element "div" :class "header"
                         :children (list (namestring (file-path (current-buffer))))))))

(defmethod (setf file-path) :around (new-val (buffer file-list-mode))
  (let ((old-val (slot-value buffer 'file-path)))
    (prog1 (call-next-method)
      (unless (equal old-val new-val)
        (with-current-buffer buffer
          (revert-buffer))))))

(defstyle file-list-mode `((".directory::after" :content "/")
                           (".header" :inherit bold)))

(defmethod focused-item ((buffer file-list-mode))
  (or (when-let (row (focused-row))
        (merge-pathnames (text-content (first-child row))
                         (file-path buffer)))
      (error "No focused file")))

(define-command file-list-find-file ()
  (find-file (focused-item (current-buffer))))
