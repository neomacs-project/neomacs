(in-package #:neomacs)

(define-mode occur-mode () ((occur-query)))

(defgeneric occur-p-aux (buffer query element)
  (:documentation "Extension point for `occur-p'."))

(defmethod occur-p-aux ((buffer list-mode) query element)
  (let ((text-node (first-child (first-child element))))
    (when-let (start (search query (text text-node)))
      (list (list text-node start (+ start (length query)))))))

(defun occur-p (query element)
  "Test if ELEMENT matches QUERY in BUFFER.

Returns either t (when highlight information is unavailable), or a
list with items of the form (text-node start end), where [start,end)
is the matched range in text-node."
  (occur-p-aux (current-buffer) query element))

(defmethod (setf occur-query) :around (new-val (buffer occur-mode))
  (let ((old-val (slot-value buffer 'occur-query)))
    (prog1 (call-next-method)
     (unless (equal old-val new-val)
       (with-current-buffer buffer
         (update-occur buffer))))))

(defun update-occur (buffer)
  (evaluate-javascript
   (ps:ps
     (ps:chain -c-s-s highlights (set "occur" (ps:new (-highlight)))))
   buffer)
  (iter (for c in (children (restriction buffer)))
    (if-let (matches (occur-p (occur-query buffer) c))
        (progn
          (remove-class c "invisible")
          (when (listp matches)
            (evaluate-javascript
             (ps:ps
               (let ((highlight-range
                       (lambda (node start end)
                         (let ((range (ps:new (-range))))
                           (ps:chain range (set-start node start))
                           (ps:chain range (set-end node end))
                           (ps:chain -c-s-s highlights
                                     (get "occur")
                                     (add range))))))
                 (ps:lisp
                  `(progn
                     ,@(iter (for (text-node start end) in matches)
                         (collect `(highlight-range (js-node ,text-node) ,start ,end)))))))
             buffer)))
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

(defstyle occur-mode `(("::highlight(occur)" :inherit match)))
