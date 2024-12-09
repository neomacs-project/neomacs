(in-package #:neomacs)

(defvar *download-items* (make-hash-table :test 'equal))

(defvar *download-list-update-helper* nil)

(defun merge-alist (new old)
  (iter (for (k . v) in new)
    (setf (assoc-value old k) v))
  old)

(setf (gethash "new-download" *event-handler-table*)
      (lambda (buffer event)
        (declare (ignore buffer))
        (let ((id (assoc-value event :id)))
          (message "Starting download ~a" (assoc-value event :url))
          (setf (gethash id *download-items*)
                event)
          (run-in-helper '*download-list-update-helper* 'update-download-list)))

      (gethash "download-updated" *event-handler-table*)
      (lambda (buffer event)
        (declare (ignore buffer))
        (let ((id (assoc-value event :id)))
          (setf (gethash id *download-items*)
                (merge-alist event (gethash id *download-items*))))
        (run-in-helper '*download-list-update-helper* 'update-download-list))

      (gethash "download-done" *event-handler-table*)
      (lambda (buffer event)
        (declare (ignore buffer))
        (let* ((id (assoc-value event :id))
               (item (setf (gethash id *download-items*)
                           (merge-alist event (gethash id *download-items*)))))
          (message "Download ~a ~a" (assoc-value item :url)
                   (assoc-value item :state)))
        (run-in-helper '*download-list-update-helper* 'update-download-list)))

(define-mode download-list-mode (list-mode) ())

(define-keys download-list-mode
  "p" 'download-list-toggle-pause
  "c" 'download-list-cancel)

(defmethod generate-rows ((buffer download-list-mode))
  (iter (for v in (sort (alex:hash-table-values *download-items*) #'>
                        :key (lambda (v) (or (assoc-value v :start-time) 0))))
    (for path = (uiop:parse-native-namestring (assoc-value v :path)))
    (collecting
      (attach-presentation
       (dom `(:tr (:td ,(str:concat (pathname-name path) "." (pathname-type path)))
                  (:td ,(str:concat
                         (sera:format-file-size-human-readable nil (assoc-value v :received)) "/"
                         (sera:format-file-size-human-readable nil (assoc-value v :total))))
                  (:td ,(assoc-value v :state))
                  (:td ,(assoc-value v :url))))
       (assoc-value v :id)))))

(defun update-download-list ()
  (when-let (buffer (get-buffer "*downloads*"))
    (with-current-buffer buffer
      (revert-buffer))))

(define-command list-downloads ()
  (switch-to-buffer
   (get-buffer-create "*downloads*" :mode 'download-list-mode)))

(define-command download-list-toggle-pause
  :mode download-list-mode ()
  (let* ((id (presentation-at (focus) 'string t))
         (state (assoc-value (gethash id *download-items*) :state)))
    (evaluate-javascript
     (format nil "Ceramic.downloads[~S].~A()"
             id (cond ((equal state "progressing")
                       "pause")
                      ((member state '("paused" "interrupted") :test 'equal)
                       "resume")
                      (t (user-error "Cannot pause or resume"))))
     :global)))

(define-command download-list-cancel
  :mode download-list-mode ()
  (let* ((id (presentation-at (focus) 'string t)))
    (evaluate-javascript
     (format nil "Ceramic.downloads[~S].cancel()" id)
     :global)))
