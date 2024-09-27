(in-package :neomacs)

;;; Positions

(eval-always
  (defstruct (end-pos (:constructor end-pos (node)))
    (node))

  (defstruct (text-pos (:constructor text-pos (node offset)))
    (node) (offset)))

(deftype pos ()
  '(or element text-pos end-pos null))

(defmethod host ((pos end-pos))
  (host (end-pos-node pos)))

(defmethod host ((pos text-pos))
  (host (text-pos-node pos)))

(defun resolve-marker (marker-or-pos)
  (if (marker-p marker-or-pos)
      (pos marker-or-pos)
      marker-or-pos))

(defun node-after (marker-or-pos)
  "Return the node after MARKER-OR-POS."
  (let ((pos (resolve-marker marker-or-pos)))
    (ematch pos
      ((element) pos)
      ((end-pos) nil)
      ((text-pos node offset) (aref (text node) offset))
      (nil))))

(defun node-before (marker-or-pos)
  "Return the node before MARKER-OR-POS."
  (let ((pos (resolve-marker marker-or-pos)))
    (labels ((handle-text-node (node)
               (if (text-node-p node)
                   (last-elt (text node))
                   node)))
      (ematch pos
        ((element) (handle-text-node (previous-sibling pos)))
        ((end-pos node) (handle-text-node (last-child node)))
        ((text-pos node offset)
         (if (> offset 0)
             (aref (text node) (1- offset))
             (handle-text-node (previous-sibling node))))
        (nil)))))

(defun node-containing (marker-or-pos)
  "Return the node containing MARKER-OR-POS."
  (let ((pos (resolve-marker marker-or-pos)))
    (ematch pos
      ((element) (parent pos))
      ((end-pos node) node)
      ((text-pos node) (parent node))
      (nil))))

(defun normalize-node-pos (node direction)
  "Convert NODE to `text-pos' if necessary.
If DIRECTION is nil, convert to leftmost position;
otherwise, convert to rightmost position."
  (if (text-node-p node)
      (text-pos node (if direction (1- (length (text node))) 0))
      node))

(defun pos-right (pos &key destructive)
  "Return the position to the right of POS.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (setq pos (resolve-marker pos))
  (labels ((node-right (node)
             (or (normalize-node-pos (next-sibling node) nil)
                 (end-pos (parent node)))))
    (ematch pos
      ((element) (node-right pos))
      ((end-pos) nil)
      ((text-pos node offset)
       (if (< (1+ offset) (length (text node)))
           (if destructive
               (progn
                 (incf (text-pos-offset pos))
                 pos)
               (text-pos node (1+ offset)))
           (node-right node)))
      (nil))))

(defun pos-left (pos &key destructive)
  "Return the position to the left of POS.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (setq pos (resolve-marker pos))
  (labels ((node-left (node)
             (normalize-node-pos (previous-sibling node) t)))
    (ematch pos
      ((element) (node-left pos))
      ((end-pos node)
       (normalize-node-pos (last-child node) t))
      ((text-pos node offset)
       (if (> offset 0)
           (if destructive
               (progn
                 (decf (text-pos-offset pos))
                 pos)
               (text-pos node (1- offset)))
           (node-left node)))
      (nil))))

(defun pos-up (pos)
  "Return the parent position of POS."
  (setq pos (resolve-marker pos))
  (when pos
    (let ((node (ematch pos
                  ((element) (parent pos))
                  ((end-pos node) node)
                  ((text-pos node) (parent node)))))
      (unless (eql node (restriction (host pos)))
        node))))

(defun pos-down (pos)
  "Return the first child position of POS."
  (setq pos (resolve-marker pos))
  (etypecase pos
    (element
     (or (normalize-node-pos (first-child pos) nil)
         (end-pos pos)))
    ((or text-pos end-pos) nil)
    (null)))

(defun pos-down-last (pos)
  "Return the last child position of POS."
  (setq pos (resolve-marker pos))
  (etypecase pos
    (element (end-pos pos))
    ((or text-pos end-pos) nil)
    (null)))

(defun pos-next (pos &key destructive)
  "Return the next position of POS in preorder traversal.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (setq pos (resolve-marker pos))
  (or (pos-down pos)
      (pos-right pos :destructive destructive)
      (when-let (up (pos-up pos))
        (pos-right up))))

(defun pos-prev (pos &key destructive)
  "Return the previous position of POS in preorder traversal.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (setq pos (resolve-marker pos))
  (or (if-let (left (pos-left pos :destructive destructive))
        (or (pos-down-last left) left)
        (pos-up pos))))

(declaim (inline iterate-pos-until))

(defun iterate-pos-until (function pos predicate &rest args)
  (iter (setq pos (apply function pos args))
    (while pos)
    (until (funcall predicate pos)))
  pos)

(defun pos-next-until (pos predicate &key destructive)
  "Return the first position after POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-until #'pos-next pos predicate :destructive destructive))

(defun pos-prev-until (pos predicate &key destructive)
  "Return the last position before POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-until #'pos-prev pos predicate :destructive destructive))

(defun pos-right-until (pos predicate &key destructive)
  "Return the first sibling after POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-until #'pos-right pos predicate :destructive destructive))

(defun pos-left-until (pos predicate &key destructive)
  "Return the last sibling before POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-until #'pos-left pos predicate :destructive destructive))

(defun pos-up-until (pos predicate)
  "Return the first parent of POS satisfying PREDICATE."
  (iterate-pos-until #'pos-up pos predicate))

(declaim (inline iterate-pos-ensure))

(defun iterate-pos-ensure (function pos predicate &rest args)
  (setq pos (resolve-marker pos))
  (iter (until (funcall predicate pos))
    (setq pos (apply function pos args))
    (while pos))
  pos)

(defun pos-next-ensure (pos predicate &key destructive)
  "Return POS or the first position after POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-ensure #'pos-next pos predicate :destructive destructive))

(defun pos-prev-ensure (pos predicate &key destructive)
  "Return POS or the last position before POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-ensure #'pos-prev pos predicate :destructive destructive))

(defun pos-right-ensure (pos predicate &key destructive)
  "Return POS or the first sibling after POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-ensure #'pos-right pos predicate :destructive destructive))

(defun pos-left-ensure (pos predicate &key destructive)
  "Return POS or the last sibling before POS satisfying PREDICATE.
If DESTRUCTIVE is non-nil, POS might be mutated."
  (iterate-pos-ensure #'pos-left pos predicate :destructive destructive))

(defun pos-up-ensure (pos predicate)
  "Return POS or the first parent of POS satisfying PREDICATE."
  (iterate-pos-ensure #'pos-up pos predicate))

(macrolet ((define-npos (symbol &rest args)
             (let ((npos (alex:symbolicate "N" symbol)))
               `(progn
                  (declaim (inline ,npos))
                  (defun ,npos (,@args)
                    ,(let ((*print-case* :downcase))
                       (format nil "Destructive version of `~a'."
                               symbol))
                    (,symbol ,@args :destructive t))))))
  (define-npos pos-left pos)
  (define-npos pos-right pos)
  (define-npos pos-next pos)
  (define-npos pos-prev pos)
  (define-npos pos-next-until pos predicate)
  (define-npos pos-prev-until pos predicate)
  (define-npos pos-left-until pos predicate)
  (define-npos pos-right-until pos predicate)
  (define-npos pos-next-ensure pos predicate)
  (define-npos pos-prev-ensure pos predicate)
  (define-npos pos-left-ensure pos predicate)
  (define-npos pos-right-ensure pos predicate))

;;; Marker

(eval-always
  (defstruct (%start-pos (:constructor %start-pos (node)))
    (node))

  (defstruct (%after-pos (:constructor %after-pos (before)))
    (before))

  (defclass marker ()
    ((pos :initarg :pos))))

(defmethod host ((pos %start-pos))
  (host (%start-pos-node pos)))

(defmethod host ((pos %after-pos))
  (host (%after-pos-before pos)))

(defmethod host ((m marker))
  (host (slot-value m 'pos)))

(defun copy-marker (marker)
  (make-instance 'marker :pos (slot-value marker 'pos)))

(defmethod initialize-instance :after ((m marker) &key)
  (push m (markers (host (slot-value m 'pos)))))

(declaim (inline marker-p))

(defun marker-p (object)
  (typep object 'marker))

(defun advance-p (marker)
  (etypecase (slot-value marker 'pos)
    ((or element end-pos text-pos) t)
    ((or %start-pos %after-pos) nil)))

(defun (setf advance-p) (new-val marker)
  (check-type new-val boolean)
  (unless (eql new-val (advance-p marker))
    (setf (slot-value marker 'pos)
          (pos-to-advance-p (pos marker) new-val)))
  new-val)

(defun pos-to-advance-p (pos advance-p)
  (if advance-p pos
      (if-let (left (npos-left pos))
        (%after-pos left)
        (%start-pos (node-containing pos)))))

(defun copy-pos (marker-or-pos)
  (etypecase marker-or-pos
    (marker (pos marker-or-pos))
    (element marker-or-pos)
    (end-pos (copy-end-pos marker-or-pos))
    (text-pos (copy-text-pos marker-or-pos))))

(defmethod pos ((m marker))
  (let ((pos (slot-value m 'pos)))
    (ematch pos
      ((or (element) (end-pos) (text-pos))
       (copy-pos pos))
      ((%start-pos node)
       (or (normalize-node-pos (first-child node) nil)
           (end-pos node)))
      ((%after-pos before) (pos-right before)))))

(defun focus-marker-p (marker)
  "Test if MARKER is the focus marker of some `neomacs-mode' instance.
Return that instance or nil otherwise."
  (let ((host (host marker)))
    (when (eq marker (focus-marker host))
      host)))

(defvar *atomic-motion-markers* nil)

(defun call-with-atomic-motion (marker thunk)
  (if (member marker *atomic-motion-markers*)
      (funcall thunk)
      (let ((saved (copy-marker marker))
            success)
        (unwind-protect
            (multiple-value-prog1
                (let ((*atomic-motion-markers*
                        (cons marker *atomic-motion-markers*)))
                  (funcall thunk))
              (setq success t))
          (when success
            (when-let (host (focus-marker-p marker))
              (nhooks:run-hook (focus-move-hook host)
                               (pos saved) (pos marker))))
          (unless success
            (setf (slot-value marker 'pos) (slot-value saved 'pos)))
          (delete-marker saved)))))

(defmacro with-atomic-motion (marker &body body)
  `(call-with-atomic-motion ,marker (lambda () ,@body)))

(defmethod (setf pos) (new-val (m marker))
  (setq new-val (copy-pos new-val))
  (when-let (host (focus-marker-p m))
    (unless (member m *atomic-motion-markers*)
      (nhooks:run-hook (focus-move-hook host) (pos m) new-val)))
  (setf (slot-value m 'pos) (pos-to-advance-p new-val (advance-p m)))
  new-val)

(defmethod print-object ((marker marker) stream)
  (print-unreadable-object (marker stream :type t :identity t)
    (format stream "~a" (slot-value marker 'pos))))

(defun delete-marker (marker)
  (alex:deletef (markers (host marker)) marker))

(defmacro with-marker ((marker marker-or-pos &optional (advance-p t))
                       &body body)
  "Make a temporary marker at MARKER-OR-POS and bind to MARKER during BODY."
  `(let ((,marker (make-instance 'marker
                                 :pos (resolve-marker ,marker-or-pos))))
     (unwind-protect
         (progn
           (setf (advance-p ,marker) ,advance-p)
           ,@body)
       (delete-marker ,marker))))

;;; Motion

(defun selectable-p (pos)
  (when pos
    (hooks:run-hook (selectable-p-hook (host pos))
                    (constantly nil) pos)))

(defun word-character-p (neomacs node)
  (and (characterp node)
       (not (member node (word-boundary-list neomacs)))))

(defun word-start-p (pos)
  (and (word-character-p (host pos) (node-after pos))
       (not (word-character-p (host pos) (node-before pos)))))

(defun word-end-p (pos)
  (and (word-character-p (host pos) (node-before pos))
       (not (word-character-p (host pos) (node-after pos)))))

(define-condition motion-error (error) ())

(define-condition end-of-subtree (motion-error) ()
  (:report "End of subtree."))

(define-condition beginning-of-subtree (motion-error) ()
  (:report "Beginning of subtree."))

(define-condition top-of-subtree (motion-error) ()
  (:report "Top of subtree."))

(define-condition leaf-of-subtree (motion-error) ()
  (:report "Leaf of subtree."))

(define-command forward-node (&optional (marker (focus)))
  "Move to closest selectable preorder successor."
  (setf (pos marker)
        (or (npos-next-until (pos marker) #'selectable-p)
            (error 'top-of-subtree))))

(define-command backward-node (&optional (marker (focus)))
  "Move to closest selectable preorder predecessor."
  (setf (pos marker)
        (or (npos-prev-until (pos marker) #'selectable-p)
            (error 'top-of-subtree))))

(define-command forward-element (&optional (marker (focus)))
  "Move after first element to the right."
  (setf (pos marker)
        (or (iterate-pos-until
             (alex:disjoin #'npos-right
                           (alex:compose #'pos-right #'pos-up))
             (pos marker)
             (alex:compose #'element-p #'node-before))
            (error 'top-of-subtree))))

(define-command backward-element (&optional (marker (focus)))
  "Move to first element to the left."
  (setf (pos marker)
        (or (iterate-pos-until
             (alex:disjoin #'npos-left #'pos-up)
             (pos marker)
             #'element-p)
            (error 'top-of-subtree)))
  (setq *adjust-marker-direction* 'backward))

(define-command beginning-of-buffer (&optional (marker (focus)))
  "Move to beginning of buffer."
  (setf (pos marker) (pos-down (restriction (host marker)))))

(define-command end-of-buffer (&optional (marker (focus)))
  "Move to end of buffer."
  (setq *adjust-marker-direction* 'backward)
  (setf (pos marker) (end-pos (restriction (host marker)))))

(defun ensure-selectable (marker &optional backward)
  (let ((pos (pos marker)))
    (unless (selectable-p pos)
      (setq pos (if backward
                    (npos-prev-until pos #'selectable-p)
                    (npos-next-until pos #'selectable-p)))
      (setf (pos marker) (or pos (error 'top-of-subtree))))))

(define-command backward-up-node (&optional (marker (focus)))
  "Move to closest selectable parent."
  (setf (pos marker) (or (pos-up-until (pos marker) #'selectable-p)
                         (error 'top-of-subtree))))

(define-command forward-word (&optional (marker (focus)))
  "Move to next word end position."
  (let ((pos (pos marker)))
    (setq pos (npos-next-until pos #'word-end-p))
    (setf (pos marker) (or pos (error 'top-of-subtree)))))

(define-command backward-word (&optional (marker (focus) non-interactive))
  "Move to previous word start position."
  (let ((pos (pos marker)))
    (setq pos (npos-prev-until pos #'word-start-p))
    (setf (pos marker) (or pos (error 'top-of-subtree)))
    (unless non-interactive
      (setq *adjust-marker-direction* 'backward))))

(define-command beginning-of-line (&optional (marker (focus)))
  (let ((pos (pos marker)))
    (iter (until (new-line-node-p (node-before pos)))
      (setq pos (or (npos-prev pos) (return))))
    (setf (pos marker) (or pos (error 'top-of-subtree)))))

(define-command end-of-line (&optional (marker (focus) non-interactive))
  (let ((pos (pos marker)))
    (iter (until (new-line-node-p (node-after pos)))
      (setq pos (or (npos-next pos) (return))))
    (setf (pos marker) (or pos (error 'top-of-subtree)))
    (unless non-interactive
      (setq *adjust-marker-direction* 'backward))))

(define-command beginning-of-defun (&optional (marker (focus)))
  "Move to current or previous toplevel node."
  (let (moved)
    (handler-case
        (iter
          (backward-up-node marker)
          (setq moved t))
      (top-of-subtree ()))
    (unless moved
      (let ((pos (pos marker)))
        (setq pos (npos-left-until
                   pos (alex:compose #'not #'new-line-node-p
                                     #'node-after)))
        (setf (pos marker) (or pos (error 'top-of-subtree)))))))

(define-command end-of-defun (&optional (marker (focus)))
  "Move to next toplevel node."
  (handler-case (iter (backward-up-node marker))
    (top-of-subtree ()))
  (let ((pos (pos marker)))
    (setq pos (npos-right-until
               pos (alex:compose #'not #'new-line-node-p
                                 #'node-after)))
    (setf (pos marker) (or pos (error 'top-of-subtree)))))
