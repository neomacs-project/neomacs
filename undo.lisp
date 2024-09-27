(in-package :neomacs)

(define-mode undo-mode ()
  "Enable undo."
  ((undo-entry (make-instance 'undo-root) :type undo-entry)
   (amalgamate-limit 20 :type integer)
   (amalgamate-count 0 :type integer)
   (keyscheme-map
    (keymaps:define-keyscheme-map "undo" ()
      keyscheme:emacs
      '("C-x u" undo-history)))))

(defmethod enable ((mode undo-mode) &key)
  (hooks:add-hook (pre-command-hook (find-submode 'neomacs-mode))
                  'undo-boundary))

(defmethod disable ((mode undo-mode) &key)
  (when-let (neomacs (find-submode 'neomacs-mode))
    (hooks:remove-hook (pre-command-hook neomacs)
                       'undo-boundary)))

(define-mode active-undo-mode ()
  "Transient mode when undo history panel is active."
  ((id-table (make-hash-table))
   (keyscheme-map
    (keymaps:define-keyscheme-map "active-undo" ()
      keyscheme:default
      '("up" undo-command
        "down" redo-command
        "left" previous-branch
        "right" next-branch
        "escape" undo-history
        "return" undo-history)
      keyscheme:emacs
      '("p" undo-command
        "n" redo-command
        "f" next-branch
        "b" previous-branch
        "q" undo-history
        "C-g" undo-history))))
  (:toggler-command-p nil))

(defmethod enable ((mode active-undo-mode) &key)
  (setf (read-only-p (find-submode 'neomacs-mode (buffer mode))) t))

(defmethod disable ((mode active-undo-mode) &key)
  (setf (read-only-p (find-submode 'neomacs-mode (buffer mode))) nil))

(define-class undo-entry ()
  ((children :type (list-of undo-entry))))

(define-class undo-root (undo-entry) ())

(define-class undo-child (undo-entry)
  ((parent (error "Must supply :parent.") :type (or undo-root undo-entry))
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

(defvar *inhibit-record-undo* nil)

(defun record-undo (undo-thunk redo-thunk)
  (unless *inhibit-record-undo*
    (when-let (mode (find-submode 'undo-mode))
      (let ((entry (undo-entry mode)))
        (unless (leaf-p entry)
          (setf entry (make-instance 'undo-child :parent entry))
          (setf (undo-entry mode) entry))
        (push undo-thunk (undo-thunks entry))
        (push redo-thunk (redo-thunks entry))
        nil))))

(defun undo-boundary (&optional (mode (find-submode 'undo-mode)))
  (let ((entry (undo-entry mode)))
    (when (leaf-p entry)
      (unless (undo-boundary-p entry)
        (setf (undo-entry mode)
              (make-instance 'undo-child :parent entry))))
    nil))

(defun remove-undo-boundary (mode)
  (let ((entry (undo-entry mode)))
    (when (undo-boundary-p entry)
      (setf (undo-entry mode) (parent entry))
      (alex:deletef (children (parent entry)) entry)
      nil)))

(defun undo-auto-amalgamate ()
  (when-let (neomacs (find-submode 'neomacs-mode))
    (when-let (undo (find-submode 'undo-mode))
      (if (and (last-command neomacs)
               (eql (name (last-command neomacs))
                    (name (this-command neomacs)))
               (< (1+ (amalgamate-count undo))
                  (amalgamate-limit undo)))
          (progn
            (remove-undo-boundary undo)
            (incf (amalgamate-count undo)))
          (setf (amalgamate-count undo) 0))))
  nil)

(defun undo (&optional (mode (find-submode 'undo-mode)))
  (let ((entry (undo-entry mode))
        (*inhibit-record-undo* t))
    (setf (undo-entry mode)
          (or (parent entry) (error "No more undo.")))
    (mapc #'funcall (undo-thunks entry))
    nil))

(defun redo (&optional (branch-index 0) (mode (find-submode 'undo-mode)))
  (let* ((entry (nth branch-index (children (undo-entry mode))))
         (*inhibit-record-undo* t))
    (if entry
        (setf (undo-entry mode) entry)
        (error "No more redo."))
    (mapc #'funcall (reverse (redo-thunks entry)))
    nil))

(define-command undo-command (&optional (mode (find-submode 'undo-mode)))
  (remove-undo-boundary mode)
  (undo mode)
  (update-undo-history))

(define-command redo-command (&optional (mode (find-submode 'undo-mode)))
  (redo 0 mode)
  (update-undo-history))

(define-command next-branch (&optional (mode (find-submode 'undo-mode)))
  (let* ((entry (undo-entry mode))
         (parent (or (parent entry)
                     (error "No next branch.")))
         (current-index (position entry (children parent))))
    (unless (< (1+ current-index) (length (children parent)))
      (error "No next branch."))
    (undo mode)
    (redo (1+ current-index) mode)
    (update-undo-history)))

(define-command previous-branch (&optional (mode (find-submode 'undo-mode)))
  (let* ((entry (undo-entry mode))
         (parent (or (parent entry)
                     (error "No next branch.")))
         (current-index (position entry (children parent))))
    (unless (> current-index 0)
      (error "No previous branch."))
    (undo mode)
    (redo (1- current-index) mode)
    (update-undo-history)))

(defun update-undo-history ()
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

(define-panel-command undo-history () (buffer "*undo*")
  (let ((parent-buffer (current-buffer)))
    (enable-modes* 'active-undo-mode parent-buffer)
    (hooks:add-hook (buffer-delete-hook buffer)
                    (lambda (buffer)
                      (declare (ignore buffer))
                      (disable-modes* 'active-undo-mode parent-buffer))))
  (setf (style buffer)
        (theme:themed-css (theme *browser*)
          ("body" :whitespace "pre"
                  :line-height 0
                  :font-family "dejavu sans mono")
          (".col" :display "inline-block"
                  :vertical-align "top")
          (".hr" :border-bottom "1px solid"
                 :margin-right "0.25em"
                 :margin-left "0.25em")
          (".row" :display "inline-block"
                  :margin-right "auto"
                  :vertical-align "top")
          (".node" :width "0.5em"
                   :height "0.5em"
                   :border-radius "50%"
                   :border "1px solid")
          (".focus" :background-color "#000")
          (".vert" :border-left "1px solid"
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
