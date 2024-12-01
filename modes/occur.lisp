(in-package #:neomacs)

(sera:export-always
    '(occur-query occur-p-aux occur-p search-in-elements))

(define-mode occur-mode () ((occur-query)))

(defgeneric occur-p-aux (buffer query element)
  (:documentation "Extension point for `occur-p'."))

(defun search-in-elements (query elements)
  (labels ((search-one (query)
             (block nil
               (dolist (element elements)
                 (do-dom (lambda (n)
                           (when (text-node-p n)
                             (when-let (beg (search (string-upcase query)
                                                    (string-upcase (text n))))
                               (return (list (list n beg (+ beg (length query))))))))
                   element)))))
    (iter (for word in (str:split #\space query))
      (if-let (result (search-one word))
        (appending result)
        (return)))))

(defmethod occur-p-aux ((buffer list-mode) query element)
  (search-in-elements query (list (first-child element))))

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

(defun render-match-highlight (buffer matches)
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

(defun clear-match-highlight (buffer)
  (evaluate-javascript
   (ps:ps
     (ps:chain -c-s-s highlights (set "occur" (ps:new (-highlight)))))
   buffer))

(defun update-occur (buffer)
  (clear-match-highlight buffer)
  (iter (for c in (children (restriction buffer)))
    (if-let (matches (occur-p (occur-query buffer) c))
        (progn
          (remove-class c "invisible")
          (render-match-highlight buffer matches))
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

(defsheet occur-mode `(("::highlight(occur)" :inherit match)))
