(in-package #:neomacs)

(define-mode occur-mode () ((occur-query)))

(defgeneric occur-p-aux (buffer query element)
  (:documentation "Extension point for `occur-p'.

Test if ELEMENT matches QUERY in BUFFER. Returns a list similar to
`ppcre:all-matches', i.e. a list of (start-1 end-1 start-2 end-2...),
where [start-n,end-n) are matched ranges."))

(defmethod occur-p-aux ((buffer list-mode) query element)
  (when-let (start (search query (text (first-child (first-child element)))))
    (list start (+ start (length query)))))

(defun occur-p (query element)
  "Test if ELEMENT matches QUERY in current buffer. Returns a list
similar to `ppcre:all-matches', i.e. a list of (start-1 end-1 start-2
end-2...), where [start-n,end-n) are matched ranges."
  (occur-p-aux (current-buffer) query element))

(defmethod (setf occur-query) :around (new-val (buffer occur-mode))
  (let ((old-val (slot-value buffer 'occur-query)))
    (prog1 (call-next-method)
     (unless (equal old-val new-val)
       (with-current-buffer buffer
         (update-occur))))))

(defun update-occur ()
  (evaluate-javascript
   (ps:ps
     (ps:chain -c-s-s highlights (set "occur" (ps:new (-highlight)))))
   (current-buffer))
  (iter (for c in (children (restriction (current-buffer))))
    (if-let (matches (occur-p (occur-query (current-buffer)) c))
        (progn
          (remove-class c "invisible")
          (evaluate-javascript
           (ps:ps
             (let* ((text-node (js-node-1 (first-child (first-child c))))
                    (highlight-range
                      (lambda (start end)
                        (let ((range (ps:new (-range))))
                          (ps:chain range (set-start text-node start))
                          (ps:chain range (set-end text-node end))
                          (ps:chain -c-s-s highlights
                                    (get "occur")
                                    (add range))))))
               (ps:lisp
                `(progn
                   ,@(iter (for (start end) on matches by #'cddr)
                       (collect `(highlight-range ,start ,end)))))))
           (current-buffer)))
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
