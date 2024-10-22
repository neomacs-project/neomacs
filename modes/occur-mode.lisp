(define-class occur-mode () ((occur-query)))

(defgeneric occur-p-aux (buffer query element))

(defmethod occur-p-aux ((buffer list-mode) query element)
  (search query (text (first-child (first-child element)))))

(defun occur-p (query element)
  (occur-p-aux (current-buffer) query element))

(defmethod (setf occur-query) (new-val (buffer occur-mode))
  (let ((old-val (slot-value buffer 'occur-query)))
    (setf (slot-value buffer 'occur-query) new-val)
    (unless (equal old-val new-val)
      (with-current-buffer buffer
        (update-occur))))
  new-val)

(defun update-occur ()
  (iter (for c in (children (restriction (current-buffer))))
    (if (occur-p (occur-query (current-buffer)) c)
        (remove-class c "invisible")
        (add-class c "invisible"))))

(define-command occur ()
  (if (typep (current-buffer) 'occur-mode)
      (progn
        (disable 'occur-mode)
        (iter (for c in (children (restriction (current-buffer))))
          (remove-class c "invisible")))
      (let ((query (read-from-minibuffer "Element matching: ")))
        (enable 'occur-mode)
        (setf (occur-query (current-buffer)) query))))
