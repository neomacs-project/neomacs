(in-package :neomacs)

(define-class undo-mixin ()
  ((undo-entry :initform (make-instance 'undo-root) :type undo-entry)
   (amalgamate-limit :default 20 :type integer)
   (amalgamate-count :default 0 :type integer)
   #+nil (keyscheme-map
    (keymaps:define-keyscheme-map "undo" ()
      keyscheme:emacs
      '("C-x u" undo-history))))
  (:documentation "Enable undo."))

(defmethod on-pre-command progn ((buffer undo-mixin))
  (undo-boundary))

(define-class active-undo-mode ()
  ((id-table :initform (make-hash-table))
   #+nil (keymap :default
           (define-keymap "active-undo" ()
             "arrow-up" 'undo-command
             "arrow-down" 'redo-command
             "arrow-left" 'previous-branch
             "arrow-right" 'next-branch
             "escape" 'undo-history
             "enter" 'undo-history
             "p" 'undo-command
             "n" 'redo-command
             "f" 'next-branch
             "b" 'previous-branch
             "q" 'undo-history
             "C-g" 'undo-history)))
  (:documentation "Transient mode when undo history panel is active."))

(defmethod read-only-p ((buffer active-undo-mode)) t)

(define-class undo-entry ()
  ((children :type (list-of undo-entry))))

(define-class undo-root (undo-entry) ())

(define-class undo-child (undo-entry)
  ((parent :initform (error "Must supply :parent.") :type (or undo-root undo-entry))
   (undo-thunks :type (list-of function))
   (redo-thunks :type (list-of function))))

(defmethod parent ((self undo-root)) nil)

(defmethod initialize-instance :after ((self undo-child) &key parent)
  (push self (children parent)))

(defgeneric leaf-p (undo-entry)
  (:method ((self undo-root)) nil)
  (:method ((self undo-child)) (null (children self))))

(defgeneric undo-boundary-p (undo-entry)
  (:method ((self undo-root)) nil)
  (:method ((self undo-child)) (null (undo-thunks self))))

(defvar *inhibit-record-undo* nil
  "If non-nil, `record-undo' has no effect.

This is bound to non-nil during undo process itself.")

(defun record-undo (undo-thunk redo-thunk)
  "Add UNDO-THUNK and REDO-THUNK to undo history.

If `*inhibit-record-undo*' is non-nil, do nothing instead."
  (unless *inhibit-record-undo*
    (when (typep (current-buffer) 'undo-mixin)
      (let ((entry (undo-entry (current-buffer))))
        (unless (leaf-p entry)
          (setf entry (make-instance 'undo-child :parent entry))
          (setf (undo-entry (current-buffer)) entry))
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
      (alex:deletef (children (parent entry)) entry)
      nil)))

(defun undo-auto-amalgamate ()
  "Call at the beginning of a command to amalgamate undo entry."
  (when (typep (current-buffer) 'undo-mixin)
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
  (let* ((entry (nth branch-index (children (undo-entry buffer))))
         (*inhibit-record-undo* t))
    (if entry
        (setf (undo-entry buffer) entry)
        (error "No more redo."))
    (mapc #'funcall (reverse (redo-thunks entry)))
    nil))

(define-command undo-command (&optional (buffer (current-buffer)))
  "Undo."
  (remove-undo-boundary buffer)
  (undo buffer)
  (update-undo-history))

(define-command redo-command (&optional (buffer (current-buffer)))
  "Redo."
  (redo 0 buffer)
  (update-undo-history))

(define-command next-branch (&optional (buffer (current-buffer)))
  (let* ((entry (undo-entry buffer))
         (parent (or (parent entry)
                     (error "No next branch.")))
         (current-index (position entry (children parent))))
    (unless (< (1+ current-index) (length (children parent)))
      (error "No next branch."))
    (undo buffer)
    (redo (1+ current-index) buffer)
    (update-undo-history)))

(define-command previous-branch (&optional (buffer (current-buffer)))
  (let* ((entry (undo-entry buffer))
         (parent (or (parent entry)
                     (error "No next branch.")))
         (current-index (position entry (children parent))))
    (unless (> current-index 0)
      (error "No previous branch."))
    (undo buffer)
    (redo (1- current-index) buffer)
    (update-undo-history)))

#+nil (defun update-undo-history ()
  (when-let (buffer (find-panel-buffer 'undo-history))
    (let ((id (gethash (undo-entry (find-submode 'undo-mode))
                       (id-table (find-submode 'active-undo-mode)))))
      (ffi-buffer-evaluate-javascript
       buffer
       (let (ps:*parenscript-stream*)
         (ps:ps*
          `(progn
             (ps:chain -array
                       (from
                        (ps:chain document
                                  (get-elements-by-class-name "focus")))
                       (for-each (lambda (x)
                                   (ps:chain x class-list
                                             (remove "focus")))))
             (let ((focus
                    (ps:chain document
                              (query-selector ,(format nil "[neomacs-identifier='~a']"
                                                       id)))))
              (ps:chain focus class-list (add "focus"))
               (ps:chain focus (scroll-into-view-if-needed)))
             nil)))))))

#+nil (define-panel-command undo-history () (buffer "*undo*")
  (let ((parent-buffer (current-buffer)))
    (enable-modes* 'active-undo-mode parent-buffer)
    (hooks:add-hook (buffer-delete-hook buffer)
                    (lambda (buffer)
                      (declare (ignore buffer))
                      (disable-modes* 'active-undo-mode parent-buffer))))
  (setf (style buffer)
        (lass:compile-and-write
          '("body" :whitespace "pre"
                  :line-height 0
                  :font-family "dejavu sans mono")
          '(".col" :display "inline-block"
                  :vertical-align "top")
          '(".hr" :border-bottom "1px solid"
                 :margin-right "0.25em"
                 :margin-left "0.25em")
          '(".row" :display "inline-block"
                  :margin-right "auto"
                  :vertical-align "top")
          '(".node" :width "0.5em"
                   :height "0.5em"
                   :border-radius "50%"
                   :border "1px solid")
          '(".focus" :background-color "#000")
          '(".vert" :border-left "1px solid"
                   :height "0.5em"
                   :width "100%"
                   :margin-left "0.25em")))

  (let ((mode (find-submode 'undo-mode))
        (id-table (id-table (find-submode 'active-undo-mode)))
        (id 0))
    (when (undo-boundary-p (undo-entry mode))
      (undo mode)) ;; remove undo boundary
    (let* ((entry (undo-entry mode))
           (root entry))
      (iter (until (undo-root-p root))
        (setq root (parent root)))
      (spinneret:with-html-string
        (labels ((draw (node)
                   (:div :class (if (eql entry node) "node focus" "node")
                         :neomacs-identifier (format nil "~a" id))
                   (setf (gethash node id-table) id)
                   (incf id)
                   nil)
                 (process (node root-p)
                   (let ((children
                           (remove-if #'undo-boundary-p (children node))))
                     (:div :class "row"
                           (unless root-p
                             (:div :class "vert"))
                           (draw node))
                     (cond ((cdr children)
                            (:div :class "vert")
                            (:div :class "hr"))
                           (children
                            (:div :class "vert")))
                     #+nil (:br)
                     (cond ((cdr children)
                            (dolist (c children)
                              (:div :class "col" (process c nil))))
                           (children
                            (process (only-elt children) nil))))))
          (:div :class "col" (process root t)))))))
