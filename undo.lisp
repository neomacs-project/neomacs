(in-package :neomacs)

(define-mode undo-mode ()
  ((undo-entry :initform (make-instance 'undo-root) :type undo-entry)
   (amalgamate-limit :default 20 :type integer)
   (amalgamate-count :default 0 :type integer))
  (:documentation "Enable undo.")
  (:toggler t))

(define-keys undo-mode
  "C-x u" 'undo-history)

(defmethod on-pre-command progn ((buffer undo-mode))
  (undo-boundary))

(define-mode active-undo-mode ()
  ((node-table :initform (make-hash-table))
   (undo-buffer :initform
                (make-buffer "*undo*"
                             :modes '(undo-history-mode))))
  (:documentation "Transient mode when undo history panel is active."))

(defmethod read-only-p ((buffer active-undo-mode)) t)

(defmethod window-decoration-aux ((buffer active-undo-mode))
  (let* ((node (call-next-method))
         (container (only-elt
                     (get-elements-by-class-name
                      node "vertical-child-container"))))
    (append-child
     container
     (dom `((:div :class "content"
                  :style "flex: 0 0 10em;"
                  :buffer ,(id (undo-buffer buffer))))))
    node))

(define-mode undo-history-mode () ()
  (:documentation "Mode for undo history buffers."))

(defmethod window-decoration-aux ((buffer undo-history-mode))
  (dom `((:div :class "content"
               :style "flex: 0 0 5em;"
               :buffer ,(id buffer)))))

(define-keys active-undo-mode
  "p" 'undo-command
  "n" 'redo-command
  "f" 'next-branch
  "b" 'previous-branch
  "q" 'quit-undo-history
  "C-g" 'quit-undo-history
  "enter" 'quit-undo-history)

(define-class undo-entry ()
  ((child-entries :initform nil :type (list-of undo-entry))))

(define-class undo-root (undo-entry) ())

(define-class undo-child (undo-entry)
  ((parent :initform (error "Must supply :parent.")
           :type (or undo-root undo-entry)
           :initarg :parent)
   (undo-thunks :initform nil :type (list-of function))
   (redo-thunks :initform nil :type (list-of function))))

(defmethod parent ((self undo-root)) nil)

(defmethod initialize-instance :after ((self undo-child) &key parent)
  (push self (child-entries parent)))

(defgeneric leaf-p (undo-entry)
  (:method ((self undo-root)) nil)
  (:method ((self undo-child)) (null (child-entries self))))

(defgeneric undo-boundary-p (undo-entry)
  (:method ((self undo-root)) nil)
  (:method ((self undo-child)) (null (undo-thunks self))))

(defvar *inhibit-record-undo* nil
  "If non-nil, `record-undo' has no effect.

This is bound to non-nil during undo process itself.")

(defun record-undo (undo-thunk redo-thunk buffer)
  "Add UNDO-THUNK and REDO-THUNK to BUFFER's undo history.

If `*inhibit-record-undo*' is non-nil, do nothing instead."
  (unless *inhibit-record-undo*
    (when (typep buffer 'undo-mode)
      (let ((entry (undo-entry buffer)))
        (unless (leaf-p entry)
          (setf entry (make-instance 'undo-child :parent entry))
          (setf (undo-entry buffer) entry))
        (push undo-thunk (undo-thunks entry))
        (push redo-thunk (redo-thunks entry))
        nil))))

(defun undo-boundary (&optional (buffer (current-buffer)))
  "Ensure undo state is at a undo boudary, insert one if necessary."
  (let ((entry (undo-entry buffer)))
    (when (leaf-p entry)
      (unless (undo-boundary-p entry)
        (setf (undo-entry buffer)
              (make-instance 'undo-child :parent entry))))
    nil))

(defun remove-undo-boundary (buffer)
  "Remove undo boudary (if any) at current undo state."
  (let ((entry (undo-entry buffer)))
    (when (undo-boundary-p entry)
      (setf (undo-entry buffer) (parent entry))
      (alex:deletef (child-entries (parent entry)) entry)
      nil)))

(defun undo-auto-amalgamate ()
  "Call at the beginning of a command to amalgamate undo entry."
  (when (typep (current-buffer) 'undo-mode)
    (if (and *last-command*
             (eql *last-command* *this-command*)
             (< (1+ (amalgamate-count (current-buffer)))
                (amalgamate-limit (current-buffer))))
        (progn
          (remove-undo-boundary (current-buffer))
          (incf (amalgamate-count (current-buffer))))
        (setf (amalgamate-count (current-buffer)) 0)))
  nil)

(defun undo (&optional (buffer (current-buffer)))
  "Move undo state up one `undo-entry'."
  (let ((entry (undo-entry buffer))
        (*inhibit-record-undo* t))
    (setf (undo-entry buffer)
          (or (parent entry) (error "No more undo.")))
    (mapc #'funcall (undo-thunks entry))
    nil))

(defun redo (&optional (branch-index 0) (buffer (current-buffer)))
  "Move undo state down to BRANCH-INDEX-th child."
  (let* ((entry (nth branch-index (child-entries (undo-entry buffer))))
         (*inhibit-record-undo* t))
    (if entry
        (setf (undo-entry buffer) entry)
        (error "No more redo."))
    (mapc #'funcall (reverse (redo-thunks entry)))
    nil))

(define-command undo-command
  :mode undo-mode (&optional (buffer (current-buffer)))
  "Undo."
  (remove-undo-boundary buffer)
  (undo buffer)
  (update-undo-history))

(define-command redo-command
  :mode undo-mode (&optional (buffer (current-buffer)))
  "Redo."
  (redo 0 buffer)
  (update-undo-history))

(define-command next-branch
  :mode undo-mode (&optional (buffer (current-buffer)))
  (let* ((entry (undo-entry buffer))
         (parent (or (parent entry)
                     (error "No next branch.")))
         (current-index (position entry (child-entries parent))))
    (unless (< (1+ current-index) (length (child-entries parent)))
      (error "No next branch."))
    (undo buffer)
    (redo (1+ current-index) buffer)
    (update-undo-history)))

(define-command previous-branch
  :mode undo-mode (&optional (buffer (current-buffer)))
  (let* ((entry (undo-entry buffer))
         (parent (or (parent entry)
                     (error "No next branch.")))
         (current-index (position entry (child-entries parent))))
    (unless (> current-index 0)
      (error "No previous branch."))
    (undo buffer)
    (redo (1- current-index) buffer)
    (update-undo-history)))

(defun update-undo-history ()
  (let ((undo-buffer (undo-buffer (current-buffer))))
    (if-let ((node (gethash (undo-entry (current-buffer))
                            (node-table (current-buffer)))))
      (with-current-buffer undo-buffer
        (setf (pos (focus)) node))
      (warn "No corresponding node for ~a in ~a."
            (undo-entry (current-buffer))
            undo-buffer))))

(define-command undo-history
  :mode undo-mode ()
  (enable 'active-undo-mode)
  (let ((undo-buffer (undo-buffer (current-buffer)))
        (node-table (node-table (current-buffer))))
    (when (undo-boundary-p (undo-entry (current-buffer)))
      (undo (current-buffer))) ;; remove undo boundary
    (let* ((entry (undo-entry (current-buffer)))
           (root entry))
      (iter (until (typep root 'undo-root))
        (setq root (parent root)))
      (with-current-buffer undo-buffer
        (labels ((draw (node pos)
                   (let ((element
                           (make-element "div" :class "node")))
                     (insert-nodes pos element)
                     (setf (gethash node node-table) element)))
                 (process (node root-p pos)
                   (let ((children
                           (remove-if #'undo-boundary-p (child-entries node)))
                         (row (make-element "div" :class "row")))
                     (insert-nodes pos row)
                     (unless root-p
                       (insert-nodes (end-pos row) (make-element "div" :class "vert")))
                     (draw node (end-pos row))
                     (when children
                       (insert-nodes pos (make-element "div" :class "vert")))
                     (when (cdr children)
                       (insert-nodes pos (make-element "div" :class "hr")))
                     (cond ((cdr children)
                            (dolist (c children)
                              (let ((col (make-element "div" :class "col")))
                                (insert-nodes pos col)
                                (process c nil (end-pos col)))))
                           (children
                            (process (only-elt children) nil pos))))))
          (let ((col (make-element "div" :class "col")))
            (insert-nodes (end-pos (document-root (current-buffer))) col)
            (process root t (end-pos col)))))
      (update-undo-history))))

(define-command quit-undo-history
  :mode active-undo-mode ()
  (disable 'active-undo-mode))

(defmethod disable-aux ((mode (eql 'active-undo-mode))
                        previous-instance)
  (delete-buffer (undo-buffer previous-instance)))

(defstyle undo-history-mode
    `(("body" :whitespace "pre"
              :line-height 0
              :font-family "dejavu sans mono")
      (".col" :display "inline-block"
              :vertical-align "top")
      (".hr" :border-bottom "1px solid currentColor"
             :margin-right "0.25em"
             :margin-left "0.25em")
      (".row" :display "inline-block"
              :margin-right "auto"
              :vertical-align "top")
      (".node" :width "0.5em"
               :height "0.5em"
               :border-radius "50%"
               :border "1px solid currentColor")
      (".focus" :background-color "currentColor")
      (".vert" :border-left "1px solid currentColor"
               :height "0.5em"
               :width "100%"
               :margin-left "0.25em")))
