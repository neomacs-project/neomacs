(in-package #:neomacs)

(define-class minibuffer-find-file-mode
    (minibuffer-completion-mode)
  ())

(define-keymap minibuffer-find-file-mode ()
  "/" 'split-node
  'complete-minibuffer 'complete-find-file
  'complete-exit-minibuffer 'complete-exit-find-file)

(defmethod update-completion-buffer ((buffer minibuffer-find-file-mode))
  (let ((path (path-before (focus))))
    (setf (file-path (completion-buffer buffer))
          (make-pathname :name nil :type nil :defaults path)
          (occur-query (completion-buffer buffer))
          (file-namestring path))))

(defmethod selectable-p-aux ((buffer minibuffer-find-file-mode) pos)
  (class-p (node-containing pos) "path-component"))

(defmethod revert-buffer-aux ((buffer minibuffer-find-file-mode))
  (call-next-method)
  (let ((last (make-element "span" :class "path-component"))
        (input (minibuffer-input-element buffer)))
    (iter (for n in (cdr (pathname-directory *default-pathname-defaults*)))
      (insert-nodes
       (end-pos input)
       (make-element "span" :class "path-component" :children (list n))))
    (insert-nodes (end-pos input) last)
    (setf (pos (focus)) (end-pos last))))

(defun path-before (&optional (pos (focus)))
  (let* ((component (node-containing pos))
         (dir (make-pathname
               :directory
               (cons ':absolute
                     (iter (for c in (child-nodes (parent component)))
                       (until (eql c component))
                       (if-let (c (text-content c))
                         (collect c into result)
                         (setq result nil))
                       (finally (return result)))))))
    (if-let (file (text-content component))
      (merge-pathnames file dir)
      dir)))

(defmethod minibuffer-input ((buffer minibuffer-find-file-mode))
  (path-before (end-pos (last-child (minibuffer-input-element buffer)))))

(define-command complete-find-file ()
  (when-let (selection (first-child (node-after (focus (completion-buffer (current-buffer))))))
    (let ((path-component (node-containing (focus))))
      (delete-nodes (pos-right path-component) nil)
      (delete-nodes (pos-down path-component) nil)
      (insert-nodes (pos-down path-component)
                    (text-content selection))
      (when (class-p selection "directory")
        (let ((new (make-element "span" :class "path-component")))
          (insert-nodes (pos-right path-component) new)
          (setf (pos (focus)) (end-pos new)))))))

(define-command complete-exit-find-file ()
  (complete-find-file)
  (exit-minibuffer))

(defstyle minibuffer-find-file-mode
    `((".path-component::before"
       :content "/")))

(defun set-auto-mode ()
  (let ((type (pathname-type (file-path (current-buffer)))))
    (cond ((uiop:directory-pathname-p (file-path (current-buffer)))
           (enable 'file-list-mode))
          ((equal type "lisp")
           (enable 'lisp-file-mode))
          (t (enable 'text-file-mode)))))

(define-command find-file
    (&optional (path
                (read-from-minibuffer
                 "Find file: "
                 :modes 'minibuffer-find-file-mode
                 :completion-buffer
                 (make-completion-buffer
                  '(file-list-mode completion-buffer-mode)))))
  ;; If PATH points to a directory, ensure it is a directory
  ;; pathname (with NIL name and type fields).
  (when-let (dir (uiop:directory-exists-p path))
    (setq path dir))
  (switch-to-buffer
   (with-current-buffer
       (make-buffer
        (if (uiop:directory-pathname-p path)
            (lastcar (pathname-directory path))
            (file-namestring path))
        :modes 'file-mode)
     (setf (file-path (current-buffer)) path)
     (set-auto-mode)
     (revert-buffer)
     (current-buffer))))

(define-class file-mode ()
  ((file-path))
  (:documentation
   "Generic mode for buffer backed by files."))

(defgeneric load-contents (file-mode)
  (:method :after ((buffer t))
    (dolist (c (child-nodes *dom-output*))
      (do-dom (alex:rcurry #'node-setup (current-buffer)) c))))

(defvar *dom-output* nil)

(defun call-with-dom-output (body)
  (if *dom-output* (funcall body)
      (let ((*dom-output* (make-instance 'element :tag-name "div")))
        (funcall body)
        (child-nodes *dom-output*))))

(defgeneric write-file (file-mode))

(define-command save-buffer (&optional (buffer (current-buffer)))
  (write-file buffer)
  (message "Wrote ~a" (file-path buffer)))

(define-keys global
  "C-x C-f" 'find-file
  "C-x C-s" 'save-buffer)
