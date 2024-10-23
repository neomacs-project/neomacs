(in-package #:neomacs)

;;; Neomacs buffer

(defvar *buffer-table* (make-hash-table) "Map ID to buffer instances.")

(defun generate-buffer-id ()
  (iter (for i from 0)
    (unless (gethash i *buffer-table*)
      (return i))))

(defvar *buffer-name-table* (make-hash-table :test 'equal)
  "Map buffer name to buffer instances.")

(defun generate-buffer-name (name)
  (if (gethash name *buffer-name-table*)
      (iter (for i from 2)
        (for new-name = (format nil "~A<~A>" name i))
        (while (gethash new-name *buffer-name-table*))
        (finally (return new-name)))
      name))

(defvar *buffer-table-lock* (bt:make-recursive-lock))

(define-class buffer (default-mixin)
  ((id :initform (generate-buffer-id) :type integer)
   (name :type string)
   (url :initarg :url :type quri:uri)
   (word-boundary-list :default (list #\  #\-))
   (focus-marker)
   (selection-marker)
   (adjust-marker-direction :initform 'forward)
   (markers :type list)
   (document-root)
   (restriction)
   (next-neomacs-id :initform 0 :accessor next-neomacs-id :type integer)
   (read-only-p :type boolean)
   (scroll-margin
    :default 0.2
    :type (real 0 0.5)
    :documentation "With value s, try to keep cursor within [s,1-s] portion of the viewport.")
   (scroll-lines :default 10 :type (integer 1))
   (styles :default (list 'buffer) :initarg :styles)
   (lock :initform (bt:make-recursive-lock) :reader lock)
   (window-decoration))
  (:default-initargs :url (quri:uri "about:blank")))

(defmethod id ((buffer buffer))
  (format nil "~A" (slot-value buffer 'id)))

(defun enable (mode-name)
  (dynamic-mixins:ensure-mix (current-buffer) mode-name))

(defun disable (mode-name)
  (dynamic-mixins:delete-from-mix (current-buffer) mode-name))

(defgeneric enable-aux (mode-name)
  (:method ((mode-name symbol)))
  (:method :after ((mode-name symbol))
    (unless (eql mode-name 'buffer)
      (when (get mode-name 'style)
        (pushnew mode-name (styles (current-buffer))))))
  (:documentation "Run when MODE-NAME is enabled.

This generic function is run with `current-buffer' bound to the buffer
for which MODE-NAME is being enabled."))

(defgeneric disable-aux (mode-name)
  (:method ((mode-name symbol))
    (when (get mode-name 'style)
      (alex:deletef (styles (current-buffer)) mode-name)))
  (:documentation "Run when MODE-NAME is disabled.

This generic function is run with `current-buffer' bound to the buffer
for which MODE-NAME is being disabled."))

(defun stable-set-difference (list-1 list-2)
  (remove-if (alex:rcurry #'member list-2) list-1))

(defmethod update-instance-for-different-class
    :after ((previous buffer) (current buffer) &key)
  (let ((previous (sb-mop:class-precedence-list (class-of previous)))
        (current (sb-mop:class-precedence-list (class-of current))))
    (dolist (old (stable-set-difference previous current))
      (disable-aux (class-name old)))
    (dolist (new (reverse (stable-set-difference current previous)))
      (enable-aux (class-name new)))))

(defun focus (&optional (buffer (current-buffer)))
  (focus-marker buffer))

(defun buffer-alive-p (buffer)
  (eql (gethash (slot-value buffer 'id) *buffer-table*) buffer))

(defvar *locked-buffers* nil)

(defun cleanup-locked-buffers ()
  (iter (for saved = *locked-buffers*)
    (dolist (buffer saved)
      (when (buffer-alive-p buffer)
        (let ((*current-buffer* buffer))
          (case (adjust-marker-direction buffer)
            ((forward) (ensure-selectable (focus buffer)))
            ((backward) (ensure-selectable (focus buffer) t)))
          (on-post-command buffer)
          (render-focus (focus buffer)))))
    (until (eq saved *locked-buffers*))))

(defun call-with-current-buffer (buffer thunk)
  (cond ((not *locked-buffers*)
         (let ((*locked-buffers* (list buffer))
               (*current-buffer* buffer))
           (bt:acquire-recursive-lock (lock buffer))
           (setf (adjust-marker-direction buffer) 'forward)
           (unwind-protect
                (with-delayed-evaluation
                  (funcall thunk))
             (unwind-protect
                  (cleanup-locked-buffers)
               (dolist (buffer *locked-buffers*)
                 (bt:release-recursive-lock (lock buffer)))))))
        ((member buffer *locked-buffers*)
         (let ((*current-buffer* buffer))
           (funcall thunk)))
        (t
         (push buffer *locked-buffers*)
         (bt:acquire-recursive-lock (lock buffer))
         (setf (adjust-marker-direction buffer) 'forward)
         (let ((*current-buffer* buffer))
           (funcall thunk)))))

(defun send-dom-update (parenscript buffer)
  (unless *inhibit-dom-update*
    (evaluate-javascript
     (let (ps:*parenscript-stream*)
       (ps:ps* parenscript))
     buffer)))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream)
    (format stream "BUFFER ~s {~a}" (name buffer) (id buffer))))

(defmethod initialize-instance :after ((buffer buffer) &key name)
  (unless name (alex:required-argument :name))
  (bt:with-recursive-lock-held (*buffer-table-lock*)
    (setf (name buffer) (generate-buffer-name name)
          (gethash (name buffer) *buffer-name-table*) buffer)
    (setf (gethash (slot-value buffer 'id) *buffer-table*) buffer))
  (cera.d:js cera.d:*driver*
             (format nil "Ceramic.createBuffer(~S, ~S, {});
Ceramic.buffers[~S].setBackgroundColor('rgba(255,255,255,0.0)');"
                     (id buffer) (quri:render-uri (url buffer))
                     (id buffer)))
  (setf (document-root buffer)
        (make-instance 'element :tag-name "body" :host buffer)
        (restriction buffer) (document-root buffer)
        (focus-marker buffer) (make-instance 'marker :pos (end-pos (document-root buffer)))
        (selection-marker buffer) (make-instance 'marker :pos (end-pos (document-root buffer)))
        (window-decoration buffer)
        (dom `((:div :class "buffer" :selectable "")
               ((:div :class "header")
                ((:div :class "header-buffer-name") ,(name buffer))
                ((:div :class "header-buffer-modes")
                      ,@(let ((modes
                                (sera:mapconcat (alex:compose #'string-downcase #'symbol-name)
                                                (modes buffer) " ")))
                          (when (> (length modes) 0)
                            (list modes)))))
               ((:div :class "content" :buffer ,(id buffer))))))
  (with-current-buffer buffer
    (dolist (new (reverse (sb-mop:class-precedence-list (class-of buffer))))
      (enable-aux (class-name new)))
    (unless (member 'common (styles buffer))
      (alex:appendf (styles buffer) (list 'common)))
    (dolist (style (styles buffer))
      (add-observer (css-cell style)
                    (nclo update-style (cell)
                      (declare (ignore cell))
                      (update-style buffer style))))))

(defgeneric on-post-command (buffer)
  (:method-combination progn)
  (:method progn ((buffer buffer))))

(defgeneric on-pre-command (buffer)
  (:method-combination progn)
  (:method progn ((buffer buffer))))

(ps:defpsmacro js-buffer (buffer)
  `(ps:getprop (ps:chain -ceramic buffers) (ps:lisp (id ,buffer))))

(defgeneric on-buffer-loaded (buffer)
  (:method-combination progn)
  (:method progn ((buffer buffer))
    (let ((marker (focus-marker buffer)))
      (setf (pos marker) (pos marker))
      (dolist (style (reverse (styles buffer)))
        (update-style buffer style)))
    (when (eql buffer (focused-buffer))
      (evaluate-javascript
       (ps:ps (ps:chain (js-buffer buffer) web-contents (focus)))
       nil))))

(defgeneric on-buffer-title-updated (buffer title)
  (:method-combination progn)
  (:method progn ((buffer buffer) (title t))))

(defgeneric on-delete-buffer (buffer)
  (:method-combination progn)
  (:method progn ((buffer buffer))))

(defgeneric on-node-setup (buffer node)
  (:method-combination progn)
  (:method progn ((buffer buffer) (node t))))

(defgeneric on-node-cleanup (buffer node)
  (:method-combination progn)
  (:method progn ((buffer buffer) (node t))))

(defgeneric on-focus-move (buffer saved new)
  (:method-combination progn)
  (:method progn ((buffer buffer) (saved t) (new t))))

(defgeneric keymaps (buffer)
  (:method-combination append)
  (:method append ((buffer buffer))
    (list *global-keymap*)))

#+nil (defgeneric compute-completion (buffer pos))

(define-command delete-buffer
    (&optional (buffer
                (get-buffer
                 (completing-read "Delete buffer: " 'buffer-list-mode))))
  (when (frame-root buffer)
    (bury-buffer buffer))
  (with-current-buffer buffer
    (on-delete-buffer buffer)
    (dolist (style (styles buffer))
      (remove-observer (css-cell style) buffer
                       :key (lambda (f) (and (typep f 'update-style)
                                             (slot-value f 'buffer)))))
    (do-dom #'node-cleanup (document-root buffer))
    (cera.d:js cera.d:*driver*
               (format nil "Ceramic.closeBuffer(~S)" (id buffer)))
    (bt:with-recursive-lock-held (*buffer-table-lock*)
      (remhash (slot-value buffer 'id) *buffer-table*)
      (remhash (name buffer) *buffer-name-table*))))

(define-command delete-this-buffer ()
  (delete-buffer (current-buffer)))

(defun make-buffer (name &rest args)
  (let ((modes (uiop:ensure-list (getf args :modes))))
    (remf args :modes)
    (apply #'make-instance
           (apply #'dynamic-mixins:mix
                  (append modes (list 'buffer)))
           :name name args)))

(defun get-buffer-create (name &rest args)
  (bt:with-recursive-lock-held (*buffer-table-lock*)
    (or (gethash name *buffer-name-table*)
        (apply #'make-buffer name args))))

(defun get-buffer (name)
  (gethash name *buffer-name-table*))

(defun rename-buffer (name)
  (bt:with-recursive-lock-held (*buffer-table-lock*)
    (remhash (name (current-buffer)) *buffer-name-table*)
    (setq name (generate-buffer-name name))
    (setf (name (current-buffer)) name)
    (setf (gethash name *buffer-name-table*) (current-buffer))
    name))

(defun modes (buffer)
  (ignore-errors
   (remove
    'buffer
    (mapcar
     #'class-name
     (slot-value (class-of buffer)
                 'dynamic-mixins::classes)))))

;;; Parenscript utils

(ps:defpsmacro js-node (node)
  (cond ((text-node-p node)
          (if-let (next (next-sibling node))
            `(ps:chain (js-node ,next) previous-sibling)
            `(ps:chain (js-node ,(parent node)) last-child)))
         ((element-p node)
          (if (equal (tag-name node) "body")
              `(ps:chain document body)
              (let ((id (attribute node "neomacs-identifier")))
                `(ps:chain document
                           (query-selector
                            ,(format nil "[neomacs-identifier='~a']" id))))))
         ((null node) nil)
         (t (error "Unknown node type ~a." node))))

(defun get-bounding-client-rect (node)
  (evaluate-javascript
   (let (ps:*parenscript-stream*)
     (ps:ps*
      `(let ((r (ps:chain (js-node ,node) (get-bounding-client-rect))))
         (list (ps:chain r x) (ps:chain r y)
               (ps:chain r width) (ps:chain r height)))))
   (host node)))

(ps:defpsmacro pos-bounding-rect (marker-or-pos)
  (let ((pos (resolve-marker marker-or-pos)))
    (ematch pos
      ((text-pos node offset)
       `(let ((range (ps:new -range))
              (node (js-node ,node)))
          (ps:chain range (set-start node ,offset))
          (ps:chain range (set-end node ,offset))
          (ps:chain range (get-bounding-client-rect))))
      ((end-pos node)
       `(ps:chain (js-node ,node) (get-bounding-client-rect))))))

(ps:defpsmacro clear-class (class-name)
  `(ps:chain -array (from (ps:chain document (get-elements-by-class-name ,class-name)))
             (for-each (lambda (e)
                         (ps:chain e class-list (remove ,class-name))))))

(ps:defpsmacro scroll-to-focus (x y)
  `(ps:chain
    window
    (scroll-to
     (+ (ps:chain window scroll-x)
        (min (- ,x (* (ps:lisp (scroll-margin (current-buffer)))
                      (ps:chain window inner-width)))
             0)
        (max (- ,x (* (ps:lisp (- 1 (scroll-margin (current-buffer))))
                      (ps:chain window inner-width)))
             0))
     (+ (ps:chain window scroll-y)
        (min (- ,y (* (ps:lisp (scroll-margin (current-buffer)))
                      (ps:chain window inner-height)))
             0)
        (max (- ,y (* (ps:lisp (- 1 (scroll-margin (current-buffer))))
                      (ps:chain window inner-height)))
             0)))))

;;; Render focus

(defun clear-focus (buffer)
  (evaluate-javascript
   (ps:ps
    (clear-class "focus")
     (clear-class "focus-tail")
     (ps:chain
      -array
      (from (ps:chain document (get-elements-by-class-name "newline")))
      (for-each
       (lambda (e)
         (let ((parent (ps:chain e parent-node))
               (newline (ps:chain document (create-element "br"))))
           (ps:chain newline
                     (set-attribute "neomacs-identifier"
                                    (ps:chain e (get-attribute
                                                 "neomacs-identifier"))))
           (ps:chain parent (replace-child newline e))))))
    (let ((highlight (ps:chain -c-s-s highlights (get "neomacs"))))
      (when highlight (ps:chain highlight (clear)))))
   buffer))

(defun render-element-focus (element)
  (evaluate-javascript
   (ps:ps
     (let* ((element (js-node-1 element))
            (rect (ps:chain element (get-bounding-client-rect))))
       (ps:chain element class-list (add "focus"))
       (scroll-to-focus (ps:chain rect left) (ps:chain rect top))))
   (host element)))

(defun render-element-focus-tail (element)
  (evaluate-javascript
   (ps:ps
     (let* ((element (js-node-1 element))
            (rect (ps:chain element (get-bounding-client-rect))))
       (ps:chain element class-list (add "focus-tail"))
       (scroll-to-focus (ps:chain rect right) (ps:chain rect bottom))))
   (host element)))

(defun render-br-focus (element)
  (evaluate-javascript
   (ps:ps
     (let* ((element (js-node-1 element))
            (parent (js-node-1 (parent element)))
            (newline (ps:chain document (create-element "div")))
            (rect (ps:chain element (get-bounding-client-rect))))
       (ps:chain newline (set-attribute "class" "newline"))
       (ps:chain newline
                 (set-attribute "neomacs-identifier"
                                (ps:chain element (get-attribute "neomacs-identifier"))))
       (ps:chain parent (replace-child newline element))
       (scroll-to-focus (ps:chain rect right) (ps:chain rect bottom))))
   (host element)))

(defun render-text-focus (pos)
  (evaluate-javascript
   (ps:ps
    (let ((range (ps:new (-range)))
          (text-node (js-node-1 (text-pos-node pos)))
          (highlight (ps:chain -c-s-s highlights (get "neomacs"))))
      (ps:chain range (set-start text-node
                                 (ps:lisp (text-pos-offset pos))))
      (ps:chain range (set-end text-node
                               (ps:lisp (1+ (text-pos-offset pos)))))
      (unless highlight
        (setq highlight (ps:new (-highlight)))
        (ps:chain -c-s-s highlights (set "neomacs" highlight)))
      (ps:chain highlight (add range))
      (let ((rect (ps:chain range (get-bounding-client-rect))))
        (scroll-to-focus (ps:chain rect left) (ps:chain rect top)))))
   (host pos)))

(defun render-focus (pos)
  (setq pos (resolve-marker pos))
  (clear-focus (host pos))
  (render-focus-aux (host pos) pos))

(defgeneric render-focus-aux (buffer pos)
  (:method ((buffer buffer) pos)
    (ematch pos
      ((element)
       (if (new-line-node-p pos)
           (render-br-focus pos)
           (render-element-focus pos)))
      ((end-pos node) (render-element-focus-tail node))
      ((text-pos) (render-text-focus pos)))))

;;; Read-only state

(define-condition read-only-error (error)
  ((buffer :initarg :buffer))
  (:report
   (lambda (c stream)
     (format stream "~a is read only." (slot-value c 'buffer)))))

(defvar *inhibit-read-only* nil)

(defun check-read-only (buffer)
  (unless *inhibit-read-only*
    (restart-case
        (when (read-only-p buffer)
          (error 'read-only-error :buffer buffer))
      (continue ()
        :report "Go on write to this buffer.")
      (continue* ()
        :report "Make buffer writable and go on."
        (setf (read-only-p buffer) nil)))))

;;; Styles

(defun update-style (buffer style)
  (let ((id (format nil "neomacs-style-~a" style)))
    (with-current-buffer buffer
      (send-dom-update
       `(let ((element (ps:chain document (get-element-by-id ,id))))
          (unless element
            (setq element (ps:chain document (create-element "style")))
            (setf (ps:chain element id) ,id)
            (ps:chain document head (append-child element)))
          (setf (ps:chain element inner-h-t-m-l)
                ,(cell-ref (css-cell style))))
       buffer))))

(defun remove-style (buffer style)
  (let ((id (format nil "neomacs-style-~a" style)))
    (with-current-buffer buffer
      (send-dom-update
       `(let ((element (ps:chain document (get-element-by-id ,id))))
          (when element
            (ps:chain element (remove))))
       buffer))))

(defmethod (setf styles) :before (new-val (buffer buffer))
  (dolist (style (stable-set-difference new-val (slot-value buffer 'styles)))
    (update-style buffer style)
    (add-observer (css-cell style)
                  (nclo update-style (cell)
                    (declare (ignore cell))
                    (update-style buffer style))))
  (dolist (style (set-difference (slot-value buffer 'styles) new-val))
    (remove-style buffer style)
    (remove-observer (css-cell style) buffer
                     :key (lambda (f) (and (typep f 'update-style)
                                           (slot-value f 'buffer))))))

(defmethod (setf name) :before (new-val (buffer buffer))
  (unless (equal new-val (slot-value buffer 'name))
    (when-let (name-field
               (car (get-elements-by-class-name
                     (window-decoration buffer)
                     "header-buffer-name")))
      (if (host name-field)
          (with-current-buffer (host name-field)
            (delete-nodes (pos-down name-field) nil)
            (insert-nodes (end-pos name-field) new-val))
          (setf (text name-field) new-val)))))

#+nil (progn
        (defstyle default `(:font-family "Verdana" :color "rgb(169,151,160)"))
 (defstyle focus `(:background-color "#f0f7ff"))
 (defstyle selection `(:background-color "#bde1ff"))
 (defstyle keyword `(:color "#fd79a8"))
 (defstyle macro `(:color "#fd79a8"))
 (defstyle special-operator `(:color "#fd79a8"))
 (defstyle string `(:color "#a29bfe"))
 (defstyle comment `(:color "#777")))

(defstyle default `(:font-family "Yomogi" :color "#54454d"))
(defstyle focus `(:background-color "rgba(169,151,160,0.1)"))
(defstyle selection `(:background-color "rgba(169,151,160,0.5)"
                      :color "#54454d"))
(defstyle keyword `(:color "#d29fa8"))
(defstyle macro `(:color "#d29fa8"))
(defstyle special-operator `(:color "#d29fa8"))
(defstyle string `(:color "#d29fa8"))
(defstyle comment `(:color "#a997a0"))
(defstyle bold `())

(defstyle focus-tail
    `(((:append "::after")
       :content " "
       :display "inline-block"
       :white-space "pre-wrap"
       :width "0.4em"
       :inherit selection)
      :inherit focus))
(defstyle cursor
    `(:position "absolute"
      :min-height "1em"
      :min-width "0.3em"
      :z-index "-1"
      :inherit selection))

(defstyle common
    `((:import (url "https://fonts.googleapis.com/css2?family=Yomogi&display=swap"))))

(defstyle doc-node `(((:append ".focus-tail::after")
                      :content "  "
                      :white-space "pre"
                      :inherit selection)
                     :left 0 :right 0
                     :white-space "pre-wrap"
                     :padding-left "1em"))

(define-class doc-mode () ())

(defstyle doc-mode `(("body" :inherit doc-node)))

(defstyle buffer
    `(("body" :inherit default)
      (".doc" :inherit doc-node)
      (".focus" :inherit focus)
      (".focus-tail" :inherit focus-tail)
      (".newline"
       :display "inline"
       :min-width "0.4em" :inherit selection)
      (".newline::after"
       :content " \\A"
       :inherit selection)
      (".invisible" :display "none")
      ("::highlight(neomacs)" :inherit cursor)
      ("#neomacs-cursor" :inherit cursor)
      ("::-webkit-scrollbar" :display "none")))
