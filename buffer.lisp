(in-package #:neomacs)

(sera:export-always
  '(buffer id name url load-status
    generate-buffer-name get-buffer buffer-alive-p
    make-buffer get-buffer-create rename-buffer
    modes keymaps
    focus adjust-marker-direction
    document-root clear-focus render-focus render-focus-aux
    selection-marker selection-active
    on-buffer-loaded on-delete-buffer on-buffer-title-updated
    on-buffer-dom-ready on-buffer-did-start-navigation
    on-post-command on-pre-command with-post-command
    on-node-setup on-node-cleanup
    enable disable toggle enable-aux disable-aux
    window-decoration frame-root
    window-decoration-aux update-window-decoration-field
    load-url with-amalgamate-js
    user-error simple-user-error
    not-supported with-demoted-errors
    read-only-error element-read-only-error
    *inhibit-read-only* check-read-only
    define-content-script))

;;; Neomacs buffer

(defvar *buffer-table* (make-hash-table)
  "Map ID (as integers) to buffer instances.")

(defun generate-buffer-id ()
  (iter (for i from 0)
    (unless (gethash i *buffer-table*)
      (return i))))

(defvar *buffer-name-table* (make-hash-table :test 'equal)
  "Map buffer name to buffer instances.")

(defun generate-buffer-name (name &optional disambiguate)
  (when (zerop (length name))
    (setq name "-"))
  (if (gethash name *buffer-name-table*)
      (progn
        (when disambiguate
          (setq name (format nil "~A<~A>" name disambiguate))
          (unless (gethash name *buffer-name-table*)
            (return-from generate-buffer-name name)))
        (iter (for i from 2)
          (for new-name = (format nil "~A<~A>" name i))
          (while (gethash new-name *buffer-name-table*))
          (finally (return new-name))))
      name))

(defvar *buffer-table-lock* (bt:make-recursive-lock))

(define-class buffer ()
  ((id :type integer)
   (name :type string)
   (url :initarg :url :type quri:uri)
   (load-status :initform :loading)
   (load-spinner :initform t)
   (word-boundary-list :default (list #\  #\- #\:))
   (focus-marker)
   (selection-marker)
   (selection-active :initform nil :type boolean)
   (adjust-marker-direction
    :initform 'forward :type (or (eql forward) (eql backward))
    :documentation
    "Preferred direction when `ensure-selectable' at the end of commands.

Can be either `forward' or `backward'.")
   (markers :initform nil :type list)
   (document-root)
   (next-neomacs-id :initform 0 :type integer)
   (scroll-margin
    :default 0.2
    :type (real 0 0.5)
    :documentation "With value s, try to keep cursor within [s,1-s] portion of the viewport.")
   (scroll-lines :default 10 :type (integer 1))
   (styles :default (list 'buffer) :initarg :styles)
   (content-scripts :default (list 'mouse-select) :initarg :content-scripts)
   (lock :initform (bt:make-recursive-lock) :reader lock)
   (post-command-thunks
    :initform nil
    :documentation "Thunks to run at the end of current command.")
   (window-decoration :initform nil)
   (frame-root :initform nil)
   (window-min-height :initform nil)
   (window-min-width :initform nil)
   (window-quit-action :initform nil)
   (amalgamate-js-p :initform nil)
   (amalgamate-js-stream :initform (make-string-output-stream)))
  (:metaclass defaultable-class)
  (:default-initargs :url "neomacs://null.contents"))

(defmethod id :around ((buffer buffer))
  (format nil "~A" (call-next-method)))

(defun enable (mode-name)
  "Enable the mode named by MODE-NAME in current buffer."
  (ensure-mix (current-buffer) mode-name))

(defun disable (mode-name)
  "Disable the mode named by MODE-NAME in current buffer.

This also disables any modes that has MODE-NAME as super-mode."
  (when-let
      (delete-modes
       (iter (for m in (modes (current-buffer)))
         (when (subtypep m mode-name)
           (collect m))))
    (apply #'delete-from-mix
           (current-buffer)
           delete-modes)))

(defun toggle (mode-name)
  "Toggle the mode named by MODE-NAME in current buffer.

This disables MODE-NAME either if it is enabled directly or via
dependency (in which case all dependents are also disabled), and
enables it otherwise."
  (if (typep (current-buffer) mode-name)
      (progn (disable mode-name) nil)
      (progn (enable mode-name) t)))

(defgeneric enable-aux (mode-name)
  (:method ((mode-name symbol)))
  (:method :after ((mode-name symbol))
    (unless (eql mode-name 'buffer)
      (when (get mode-name 'style)
        (pushnew mode-name (styles (current-buffer)))))
    (mapc #'enable (hooks mode-name)))
  (:documentation "Run after MODE-NAME is enabled.

This generic function is run with `current-buffer' bound to the buffer
for which MODE-NAME is being enabled."))

(defgeneric disable-aux (mode-name previous-instance)
  (:method ((mode-name symbol) (previous-instance t)))
  (:method :before ((mode-name symbol) (previous-instance t))
    (mapc #'disable (hooks mode-name)))
  (:documentation "Run before MODE-NAME is disabled.

This generic function is run with `current-buffer' bound to the buffer
for which MODE-NAME is being disabled."))

(defun stable-set-difference (list-1 list-2)
  (remove-if (alex:rcurry #'member list-2) list-1))

(defmethod update-instance-for-different-class
    :after ((previous-instance buffer)
            (current-instance buffer) &key)
  (let* ((previous
           (sb-mop:class-precedence-list
            (class-of previous-instance)))
         (current
           (sb-mop:class-precedence-list
            (class-of current-instance)))
         (disabled-classes (stable-set-difference previous current))
         (enabled-classes (reverse (stable-set-difference current previous))))
    (dolist (old disabled-classes)
      (disable-aux (class-name old) previous-instance))
    (dolist (new enabled-classes)
      (enable-aux (class-name new)))
    ;; Sometimes no class are add and removed but CLOS still calls us,
    ;; test for this to reduce flickering
    (when (or disabled-classes enabled-classes)
      (update-window-decoration (current-buffer)))))

(defun focus (&optional (buffer (current-buffer)))
  (focus-marker buffer))

(defun buffer-alive-p (buffer)
  (when (eql (gethash (slot-value buffer 'id) *buffer-table*)
             buffer)
    buffer))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream)
    (format stream "BUFFER ~s {~a}" (name buffer) (id buffer))))

(defnclo update-style (buffer style) (cell)
  (declare (ignore cell))
  (update-style buffer style))

(defmethod initialize-instance :after
    ((buffer buffer) &key id name disambiguate revert)
  (unless name (alex:required-argument :name))
  (when id (setf (slot-value buffer 'id) (parse-integer id)))
  (bt:with-recursive-lock-held (*buffer-table-lock*)
    ;; Initialize with slot-value instead of accessor, to avoid triggering
    ;; the before method to update window title
    (setf (slot-value buffer 'name) (generate-buffer-name name disambiguate)
          (gethash (name buffer) *buffer-name-table*) buffer)
    (unless (slot-boundp buffer 'id)
      (setf (slot-value buffer 'id) (generate-buffer-id)))
    (setf (gethash (slot-value buffer 'id) *buffer-table*) buffer))
  (unless (< (slot-value buffer 'id) 0)
    (cera.d:js cera.d:*driver*
               (format nil "Ceramic.createBuffer(~S, ~S,
{webPreferences:{preload:~s}});
Ceramic.buffers[~S].setBackgroundColor('rgba(255,255,255,0.0)');"
                       (id buffer) (url buffer)
                       (uiop:native-namestring
                        (ceramic:resource 'assets "preload.js"))
                       (id buffer))))
  (setf (document-root buffer)
        (make-instance 'element :tag-name "body" :host buffer)
        (focus-marker buffer) (make-instance 'marker :pos (end-pos (document-root buffer)))
        (selection-marker buffer) (make-instance 'marker :pos (end-pos (document-root buffer))))
  (with-current-buffer buffer
    (dolist (new (reverse (sb-mop:class-precedence-list (class-of buffer))))
      (enable-aux (class-name new)))
    (dolist (style (styles buffer))
      (add-observer (css-cell style)
                    (make-update-style buffer style)))
    (unless (member 'common (styles buffer))
      (alex:appendf (styles buffer) (list 'common)))
    (when revert
      (revert-buffer))))

(defmethod reinitialize-instance :after
    ((buffer buffer) &key revert)
  (when revert
    (with-current-buffer buffer
      (revert-buffer))))

(defgeneric on-post-command (buffer)
  (:method-combination progn :most-specific-last)
  (:method progn ((buffer buffer))
    (iter (for saved = (post-command-thunks buffer))
      (while saved)
      (setf (post-command-thunks buffer) nil)
      (mapc #'funcall (nreverse saved))))
  (:documentation "Run after command invocation.

More specifically, this runs after a command invocation iff it has
ever entered any `with-current-buffer' form for BUFFER during its
execution."))

(defgeneric on-pre-command (buffer)
  (:method-combination progn)
  (:method progn ((buffer buffer)))
  (:documentation "Run before command invocation.

More specifically, this runs before entering the first
`with-current-buffer' form for BUFFER during a command invocation."))

(defun call-with-post-command (node slots thunk)
  (let ((buffer (host node)))
    (labels ((action ()
               (when (eql (host node) buffer)
                 (funcall thunk)))
             (observer (cell)
               (declare (ignore cell))
               (push #'action (post-command-thunks buffer))))
      (dolist (slot slots)
        (add-observer (slot-value node slot) #'observer)))))

(defmacro with-post-command ((node &rest slots) &body body)
  "Run BODY at the end of current command, if any SLOTS of NODE is
changed."
  `(call-with-post-command
    ,node (list ,@slots) (lambda () ,@body)))

(ps:defpsmacro js-buffer (buffer)
  `(ps:getprop (ps:chain -ceramic buffers) (ps:lisp (id ,buffer))))

(defgeneric on-buffer-dom-ready (buffer)
  (:method-combination progn :most-specific-last)
  (:method progn ((buffer buffer))
    (let ((marker (focus-marker buffer)))
      (setf (pos marker) (pos marker)))
    (dolist (style (reverse (styles buffer)))
      (ensure-style buffer style)
      (update-style buffer style))
    (dolist (script (content-scripts buffer))
      (evaluate-javascript (get script 'content-script) buffer))
    (when (eql buffer (focused-buffer))
      (evaluate-javascript
       (ps:ps (ps:chain (js-buffer buffer) web-contents (focus)))
       :global))))

(defgeneric on-buffer-loaded (buffer url err)
  (:method-combination progn :most-specific-last)
  (:method progn ((buffer buffer) (url t) (err t)))
  (:method :around ((buffer buffer) (url t) err)
    (when err
      ;; ERROR_ABORTED (-3) comes from issueing another load before
      ;; loading completed, which is usually benign. Just silent it
      ;; for now.
      (unless (eql (assoc-value err :errno) -3)
        (message "~a failed to load URL ~a: ~a"
                 buffer url (assoc-value err :code))))
    (when (equal url (url buffer))
      (setf (load-status buffer) (if err :failed :loaded)))
    (call-next-method))
  (:documentation
   "Run when BUFFER finishes loading URL.

This is invoked both when load succeeded or failed. When load
succeeded, ERR is nil."))

(defgeneric on-buffer-title-updated (buffer title)
  (:method-combination progn :most-specific-last)
  (:method progn ((buffer buffer) (title t))))

(defgeneric on-buffer-did-start-navigation (buffer details)
  (:method-combination progn :most-specific-last)
  (:method progn ((buffer buffer) (details t))))

(defgeneric on-delete-buffer (buffer)
  (:method-combination progn)
  (:method progn ((buffer buffer))
    (dolist (old (sb-mop:class-precedence-list (class-of buffer)))
      (disable-aux (class-name old) buffer))
    (dolist (style (styles buffer))
      (remove-observer (css-cell style) buffer
                       :key (lambda (f) (and (typep f 'update-style)
                                             (slot-value f 'buffer)))))
    (do-dom #'node-cleanup (document-root buffer)))
  (:documentation
   "Run when BUFFER is about to be deleted."))

(defgeneric on-node-setup (buffer node)
  (:method-combination progn :most-specific-last)
  (:method progn ((buffer buffer) (node t)))
  (:documentation
   "Run some action when NODE is inserted into BUFFER.

NODE can be either an `element' or a `text-node'."))

(defgeneric on-node-cleanup (buffer node)
  (:method-combination progn)
  (:method progn ((buffer buffer) (node t)))
  (:documentation
   "Run some action when NODE is removed from BUFFER.

NODE can be either an `element' or a `text-node'."))

(defgeneric window-decoration-aux (buffer)
  (:method ((buffer buffer))
    (dom `(:div :class "buffer" :selectable ""
                (:div :class "header"
                      (:div :class "header-buffer-name" ,(name buffer))
                      (:div :class "header-spinner" ,@ (unless (load-spinner buffer)
                                                         '(:style "display:none;")))
                      (:div :class "header-buffer-modes"
                            ,@(let ((modes
                                      (sera:mapconcat #'lighter (modes buffer)
                                                      " ")))
                                (when (> (length modes) 0)
                                  (list modes)))))
                (:div :class "vertical-child-container"
                      (:div :class "main content" :buffer ,(id buffer))))))
  (:documentation "Create window-decoration for BUFFER.

Should return a `div' with `buffer' or `minibuffer' CSS class, which
contains a single descendant with both `main' and `content' CSS
class. This descendant must have a `buffer' CSS attribute with the
`id' of BUFFER as its value.

Child buffers can be included via other elements with `content' CSS
class."))

(defun update-window-decoration-field (buffer name text)
  "Update the element with CSS class NAME in BUFFER's window decoration.

There must be exactly one element with NAME as CSS class, whose
content is replaced with TEXT. If BUFFER is not displayed, this
function does nothing."
  (when-let* ((node (window-decoration buffer))
              (field
               (only-elt (get-elements-by-class-name node name))))
    (with-current-buffer (host field)
      (delete-nodes (pos-down field) nil)
      (insert-nodes (end-pos field) text))))

#+nil (defgeneric compute-completion (buffer pos))

(define-command delete-buffer
  :interactive
  (lambda ()
    (list (completing-read "Delete buffer: " 'buffer-list-mode)))
  (buffer)
  (cleanup-buffer-display buffer)
  (with-current-buffer buffer
    (on-delete-buffer buffer)
    (cera.d:js cera.d:*driver*
               (format nil "Ceramic.closeBuffer(~S)" (id buffer)))
    (bt:with-recursive-lock-held (*buffer-table-lock*)
      (remhash (slot-value buffer 'id) *buffer-table*)
      (remhash (name buffer) *buffer-name-table*))))

(define-command delete-this-buffer ()
  (delete-buffer (current-buffer)))

(defun make-buffer (name &rest args)
  "Create a buffer with NAME.

ARGS is passed as initialization arguments besides some extra keyword
arguments:

:mode MODE-OR-MODES: Enable MODE-OR-MODES in the new
buffer. MODE-OR-MODES can either be a list or a single symbol.

:disambiguate SUFFIX: When provided, try appending `<SUFFIX>' to NAME
in case of name collision before trying `<number>'.

:revert REVERT-P: If REVERT-P is t, call `revert-buffer' on the new
buffer."
  (let ((modes (uiop:ensure-list (getf args :mode))))
    (remf args :mode)
    (apply #'make-instance
           (apply #'mix (append modes (list 'buffer)))
           :name name args)))

(defun load-url (buffer url)
  "Load URL in BUFFER.

This set up a promise handler so that `on-buffer-loaded' is triggered
when URL finishes loading. We use this instead of Electron's
`did-finish-load' event because the latter doesn't carry url
information, and getting url with `webContents.getURL()' isn't
reliable because it may get the URL of a later issued unfinished load
operation."
  (setf (url buffer) url
        (load-status buffer) :loading)
  (evaluate-javascript
   (format nil "Ceramic.buffers[~S].webContents.loadURL(~S).then(()=>
        {RemoteJS.send(JSON.stringify({inputEvent: {type: 'load', url: ~S}, buffer: ~S}));},
        (err)=>{RemoteJS.send(JSON.stringify({inputEvent: {type: 'fail-load', url: ~S, err: err}, buffer: ~S}));});"
           (id buffer) url
           url (id buffer)
           url (id buffer))
   :global))

(defun call-with-amalgamate-js (buffer thunk)
  (if (amalgamate-js-p buffer)
      (funcall thunk)
      (unwind-protect
           (progn
             (setf (amalgamate-js-p buffer) t)
             (funcall thunk))
        (setf (amalgamate-js-p buffer) nil)
        (send-js-for-buffer
         (get-output-stream-string (amalgamate-js-stream buffer))
         buffer))))

(defmacro with-amalgamate-js (buffer &body body)
  `(call-with-amalgamate-js ,buffer
                            (lambda () ,@body)))

(defun get-buffer-create (name &rest args)
  "Returning a buffer with NAME if found, or create one.

When creating a new buffer, NAME and ARGS has the same meaning as in
`make-buffer'.

If an existing buffer is found, it is reinitialized with ARGS."
  (bt:with-recursive-lock-held (*buffer-table-lock*)
    (if-let (existing (gethash name *buffer-name-table*))
      (let ((modes (uiop:ensure-list (getf args :mode))))
        (remf args :mode)
        (change-class
         existing
         (ensure-mixin
          (apply #'mix (append modes (list 'buffer)))))
        (apply #'reinitialize-instance
               existing args))
      (apply #'make-buffer name args))))

(defun get-buffer (name)
  "Find and return a buffer with NAME, return nil if not found."
  (gethash name *buffer-name-table*))

(defun rename-buffer (name)
  (bt:with-recursive-lock-held (*buffer-table-lock*)
    (remhash (name (current-buffer)) *buffer-name-table*)
    (setq name (generate-buffer-name name))
    (setf (name (current-buffer)) name)
    (setf (gethash name *buffer-name-table*) (current-buffer))
    name))

(defun modes (buffer)
  "Return the list of explicitly enabled modes in BUFFER."
  (iter (for c in (ignore-errors
                   (slot-value (class-of buffer) 'classes)))
    (when (typep c 'mode)
      (collect (class-name c)))))

(defun keymaps (buffer)
  "Compute the list of active keymaps for BUFFER."
  (let (keymaps)
    (iter (for node first (node-containing (focus buffer))
               then (parent node))
      (while node)
      (when-let (keymap (attribute node 'keymap))
        (push keymap keymaps)))
    (iter (for c in (sb-mop:class-precedence-list (class-of buffer)))
      (when-let (keymap (keymap c))
        (push keymap keymaps)))
    (push *global-keymap* keymaps)
    (nreverse keymaps)))

;;; Parenscript utils

(defun get-bounding-client-rect (pos)
  "Get the bounding client rect of the node after or containing POS.

If POS is a `text-pos', the node is the one character after POS;
if POS is an `end-pos', the node is instead the one containing POS.

The bounding rect is returned as a list (X Y WIDTH HEIGHT). X, Y,
WIDTH and HEIGHT are numbers in pixels."
  (let ((pos (resolve-marker pos)))
    (ematch pos
      ((text-pos node offset)
       (evaluate-javascript-sync
        (ps:ps
          (let ((range (ps:new -range))
                (node (js-node-1 node)))
            (ps:chain range (set-start node (ps:lisp offset)))
            (ps:chain range (set-end node (ps:lisp (1+ offset))))
            (let ((r (ps:chain range (get-bounding-client-rect))))
              (list (ps:chain r x) (ps:chain r y)
                    (ps:chain r width) (ps:chain r height)))))
        (host node)))
      ((or (end-pos node) node)
       (evaluate-javascript-sync
        (ps:ps
          (let ((r (ps:chain (js-node-1 node) (get-bounding-client-rect))))
            (list (ps:chain r x) (ps:chain r y)
                  (ps:chain r width) (ps:chain r height))))
        (host node))))))

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
     (let ((cursor (ps:chain document (get-element-by-id "neomacs-cursor"))))
       (when cursor (ps:chain cursor (remove))))
     (let ((highlight (ps:chain -c-s-s highlights (get "neomacs"))))
       (when highlight (ps:chain highlight (clear)))))
   buffer))

(defun render-element-focus (element &optional (class "focus"))
  (evaluate-javascript
   (ps:ps
     (let* ((element (js-node-1 element))
            (rect (ps:chain element (get-bounding-client-rect))))
       (ps:chain element class-list (add (ps:lisp class)))
       (scroll-to-focus (ps:chain rect left) (ps:chain rect top))))
   (host element)))

(defun render-element-focus-tail
    (element &optional (class "focus-tail"))
  (evaluate-javascript
   (ps:ps
     (let* ((element (js-node-1 element))
            (rect (ps:chain element (get-bounding-client-rect))))
       (ps:chain element class-list (add (ps:lisp class)))
       (scroll-to-focus (ps:chain rect right) (ps:chain rect bottom))))
   (host element)))

(defun render-br-focus (pos)
  (bind (((x y w h)
          (or (get-bounding-client-rect pos)
              (return-from render-br-focus))))
    (when-let* ((prev (pos-left pos))
                (rect (get-bounding-client-rect prev)))
      (bind (((xx yy ww hh) rect))
        (unless (zerop ww)
          (setq x (+ xx ww) y yy w 0 h hh))))
    (evaluate-javascript
     (ps:ps
       (let ((cursor (ps:chain document
                               (get-element-by-id "neomacs-cursor")))
             (rect (ps:chain document document-element (get-bounding-client-rect))))
         (unless cursor
           (setq cursor (ps:chain document (create-element "div")))
           (setf (ps:chain cursor id) "neomacs-cursor")
           (ps:chain document document-element (append-child cursor)))
         (scroll-to-focus (ps:lisp (+ x w)) (ps:lisp (+ y h)))
         (setf (ps:chain cursor style left)
               (+ (- (ps:lisp x) (ps:chain rect x))
                  "px")
               (ps:chain cursor style top)
               (+ (- (ps:lisp y) (ps:chain rect y))
                  "px")
               (ps:chain cursor style width)
               (ps:lisp (format nil "~apx" w))
               (ps:chain cursor style height)
               (ps:lisp (format nil "~apx" h)))))
     (host pos))))

(defun render-text-focus
    (text-node start end &optional (highlight "neomacs"))
  (evaluate-javascript
   (ps:ps
    (let ((range (ps:new (-range)))
          (text-node (js-node-1 text-node))
          (highlight (ps:chain -c-s-s highlights
                               (get (ps:lisp highlight)))))
      (ps:chain range (set-start text-node (ps:lisp start)))
      (ps:chain range (set-end text-node (ps:lisp end)))
      (unless highlight
        (setq highlight (ps:new (-highlight)))
        (ps:chain -c-s-s highlights (set (ps:lisp highlight)
                                         highlight)))
      (ps:chain highlight (add range))
      (let ((rect (ps:chain range (get-bounding-client-rect))))
        (scroll-to-focus (ps:chain rect left) (ps:chain rect top)))))
   (host text-node)))

(defun render-focus (pos)
  (setq pos (resolve-marker pos))
  (assert (eq (host pos) (current-buffer)))
  (with-amalgamate-js (current-buffer)
    (clear-focus (host pos))
    (render-focus-aux (host pos) pos)))

(defgeneric render-focus-aux (buffer pos)
  (:method ((buffer buffer) pos)
    (ematch pos
      ((element)
       (if (new-line-node-p pos)
           (render-br-focus pos)
           (render-element-focus pos)))
      ((end-pos node) (render-element-focus-tail node))
      ((text-pos node offset)
       (if (new-line-node-p (node-after pos))
           (render-br-focus pos)
           (render-text-focus node offset (1+ offset))))))
  (:documentation
   "Render focus at POS for BUFFER"))

;;; User error

(define-condition user-error (error) ()
  (:report
   (lambda (c stream)
     (declare (ignore c))
     (write-string "User error" stream)))
  (:documentation
   "A user generated error.

This is normally treated the same as a `quit' condition and does not
drop into Neomacs debugger."))

(define-condition simple-user-error (user-error)
  ((message :initform (alex:required-argument :message)
            :initarg :message))
  (:report
   (lambda (c stream)
     (write-string (slot-value c 'message) stream))))

(define-condition not-supported (user-error)
  ((buffer :initarg :buffer)
   (operation :initarg :operation))
  (:report
   (lambda (c stream)
     (format stream "~a does not support ~a"
             (slot-value c 'buffer)
             (slot-value c 'operation)))))

(defun not-supported (buffer operation)
  (signal 'not-supported :buffer buffer :operation operation))

(defmacro with-demoted-errors (prompt &body body)
  "Run BODY and demote any error to simple messages.
Error messages are prepended with PROMPT.

If `*debug-on-error*' is t, run BODY without catching its errors."
  `(block nil
     (handler-bind
         ((error
            (lambda (c)
              (unless *debug-on-error*
                (message "~a~a" ,prompt c)
                (return)))))
       ,@body)))

(defun user-error (control-string &rest format-arguments)
  "Signal a `simple-user-error' with messages formatted from
CONTROL-STRING and FORMAT-ARGUMENT."
  (signal 'simple-user-error
          :message (apply #'format nil control-string format-arguments)))

;;; Read-only state

(define-condition read-only-error (user-error)
  ((buffer :initarg :buffer))
  (:report
   (lambda (c stream)
     (format stream "~a is read only." (slot-value c 'buffer)))))

(define-condition element-read-only-error (user-error)
  ((element :initarg :element))
  (:report
   (lambda (c stream)
     (format stream "~a is read only." (slot-value c 'element)))))

(defvar *inhibit-read-only* nil)

(defgeneric check-read-only (buffer pos)
  (:method ((buffer buffer) (pos t))
    (let ((node (node-containing pos)))
      (when (attribute node 'read-only)
        (error 'element-read-only-error :element node))))
  (:method :around ((buffer buffer) (pos t))
    (unless *inhibit-read-only*
      (restart-case
          (call-next-method)
        (continue ()
          :report "Go on write to this buffer."))))
  (:documentation
   "Check if POS in BUFFER is read-only.

If it is, should signal a condition of type `read-only-error'."))

(define-mode read-only-mode () ()
  (:documentation "Make this buffer read-only.")
  (:toggler t))

(defmethod check-read-only ((buffer read-only-mode) (pos t))
  (error 'read-only-error :buffer buffer))

;;; Mouse support

(setf (get 'mouse-select 'content-script)
      "function resolveMousePos(x,y){
    const pos = document.caretPositionFromPoint(x,y);
    if(!pos) return;
    const {offsetNode,offset} = pos;
    const clickTextNode = function (node, offset){
        const next = node.nextSibling;
        if(next){
            return {next:next.getAttribute('neomacs-identifier'),
                    offset:offset}}
        else{
            return {parent:node.parentNode.getAttribute(
                'neomacs-identifier'),
                    offset:offset}}
    }
    if(offsetNode.nodeType === 3){
        return clickTextNode(offsetNode,offset);
    }
    else if(offsetNode.nodeType === 1){
        const child = offsetNode.childNodes[offset];
        if(!child) return;
        if(child.nodeType === 3) {return clickTextNode(child,0)}
        else if(child.nodeType === 1) {
            return {element:child.getAttribute('neomacs-identifier')}}}
}
document.body.addEventListener('click',function (event){
    electronAPI.send('neomacs',{type:'click', x:event.clientX, y:event.clientY})})")

(defun resolve-mouse-pos (x y)
  (let ((data (evaluate-javascript-sync
               (format nil "resolveMousePos(~s,~s)" x y)
               (current-buffer))))
    (sera:cond-let x
      ((assoc-value data :next)
       (when-let* ((next (get-element-by-neomacs-id
                          (document-root (current-buffer))
                          x))
                   (text-node (previous-sibling next))
                   (offset (assoc-value data :offset)))
         (if (< offset (length (text text-node)))
             (text-pos text-node offset)
             next)))
      ((assoc-value data :parent)
       (when-let* ((parent (get-element-by-neomacs-id
                            (document-root (current-buffer))
                            x))
                   (text-node (last-child parent))
                   (offset (assoc-value data :offset)))
         (if (< offset (length (text text-node)))
             (text-pos text-node offset)
             (end-pos parent))))
      ((assoc-value data :element)
       (when-let* ((element (get-element-by-neomacs-id
                             (document-root (current-buffer))
                             x)))
         element)))))

(defvar *ipc-handler-table* (make-hash-table :test 'equal))

(setf (gethash "click" *ipc-handler-table*)
      (lambda (buffer details)
        (when buffer
          (with-current-buffer buffer
            (on-mouse-click buffer (assoc-value details :x)
                            (assoc-value details :y))))))

(defgeneric on-mouse-click (buffer x y)
  (:method-combination progn)
  (:method progn ((buffer buffer) x y)
    (when-let (pos (resolve-mouse-pos x y))
      (setf (pos (focus)) pos))))

;;; Styles

(defun ensure-style (buffer style)
  "Ensure STYLE exist as the last element in <header> for BUFFER."
  (let ((id (format nil "neomacs-style-~a" style)))
    (evaluate-javascript
     (ps:ps
       (let ((element (ps:chain document (get-element-by-id
                                          (ps:lisp id)))))
         (unless element
           (setq element (ps:chain document (create-element "style")))
           (setf (ps:chain element id) (ps:lisp id)))
         (ps:chain document head (append-child element))))
     buffer)))

(defun update-style (buffer style)
  "Update CSS content of STYLE in BUFFER."
  (let ((id (format nil "neomacs-style-~a" style)))
    (evaluate-javascript
     (ps:ps
       (let ((element (ps:chain document (get-element-by-id
                                          (ps:lisp id)))))
         (setf (ps:chain element inner-h-t-m-l)
               (ps:lisp (cell-ref (css-cell style))))))
     buffer)))

(defun remove-style (buffer style)
  (let ((id (format nil "neomacs-style-~a" style)))
    (evaluate-javascript
     (ps:ps
       (let ((element (ps:chain document
                                (get-element-by-id (ps:lisp id)))))
         (when element
           (ps:chain element (remove)))))
     buffer)))

(defmethod (setf styles) :before (new-val (buffer buffer))
  ;; First ensure correct order
  (dolist (style (reverse new-val))
    (ensure-style buffer style))
  (dolist (style (set-difference new-val (slot-value buffer 'styles)))
    (update-style buffer style)
    (add-observer (css-cell style)
                  (make-update-style buffer style)))
  (dolist (style (set-difference (slot-value buffer 'styles) new-val))
    (remove-style buffer style)
    (remove-observer (css-cell style) buffer
                     :key (lambda (f) (and (typep f 'update-style)
                                           (slot-value f 'buffer))))))

(defmethod (setf content-scripts) :before (new-val (buffer buffer))
  (dolist (script (reverse
                   (stable-set-difference
                    new-val
                    (slot-value buffer 'content-scripts))))
    (evaluate-javascript (get script 'content-script) buffer)))

(defmacro define-content-script (name &body body)
  `(progn
     (setf (get ',name 'content-script)
           (ps:ps ,@body))
     ',name))

(defmethod (setf name) :before (new-val (buffer buffer))
  (unless (equal new-val (slot-value buffer 'name))
    (update-window-decoration-field
     buffer "header-buffer-name" new-val)))

(defmethod (setf load-spinner) :before (new-val (buffer buffer))
  (when-let* ((node (window-decoration buffer))
              (field (car (get-elements-by-class-name node "header-spinner"))))
    (with-current-buffer (host field)
      (evaluate-javascript-sync
       (ps:ps (setf (ps:chain (js-node-1 field) style display)
                    (ps:lisp (if new-val "block" "none"))))
       (current-buffer)))))

(defstyle default `(:font-family "CMU Concrete"
                    :color "#54454d"))
(defstyle focus `(:background-color "rgba(169,151,160,0.1)"))
(defstyle selection `(:background-color "rgba(169,151,160,0.5)"
                      :color "#54454d"))
(defstyle range-selection `(:inherit selection))
(defstyle keyword `(:color "#d29fa8"))
(defstyle macro `(:inherit keyword))
(defstyle special-operator `(:inherit keyword))
(defstyle string `(:inherit comment))
(defstyle comment `(:color "#a997a0"))
(defstyle bold `(:font-weight 900))
(defstyle match `(:inherit selection))
(defstyle monospace `(:font-family "monospace"))
(defstyle link `(:inherit default))

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

(defsheet common
    `((:font-face
       :font-family "Space Mono" :font-style "normal" :font-weight "400"
       :src "url(\"neomacs://sys/space-mono/SpaceMono-Regular.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "Space Mono" :font-style "italic" :font-weight "400"
       :src "url(\"neomacs://sys/space-mono/SpaceMono-Italic.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "Space Mono" :font-style "normal" :font-weight "700"
       :src "url(\"neomacs://sys/space-mono/SpaceMono-Bold.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "Space Mono" :font-style "italic" :font-weight "700"
       :src "url(\"neomacs://sys/space-mono/SpaceMono-BoldItalic.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "CMU Concrete" :font-style "normal" :font-weight "400"
       :src "url(\"neomacs://sys/cmu-concrete/cmunorm.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "CMU Concrete" :font-style "italic" :font-weight "400"
       :src "url(\"neomacs://sys/cmu-concrete/cmunoti.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "CMU Concrete" :font-style "normal" :font-weight "700"
       :src "url(\"neomacs://sys/cmu-concrete/cmunobx.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "CMU Concrete" :font-style "italic" :font-weight "700"
       :src "url(\"neomacs://sys/cmu-concrete/cmunobi.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "DejaVu Sans Mono" :font-style "normal" :font-weight "400"
       :src "url(\"neomacs://sys/dejavu-sans-mono/DejaVuSansMono.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "DejaVu Sans Mono" :font-style "italic" :font-weight "400"
       :src "url(\"neomacs://sys/dejavu-sans-mono/DejaVuSansMono-Oblique.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "DejaVu Sans Mono" :font-style "normal" :font-weight "700"
       :src "url(\"neomacs://sys/dejavu-sans-mono/DejaVuSansMono-Bold.ttf\") format(\"truetype\")")
      (:font-face
       :font-family "DejaVu Sans Mono" :font-style "italic" :font-weight "700"
       :src "url(\"neomacs://sys/dejavu-sans-mono/DejaVuSansMono-BoldOblique.ttf\") format(\"truetype\")")))

(defstyle doc-node `(((:append ".focus-tail::after")
                      :content "  "
                      :white-space "pre"
                      :inherit selection)
                     :left 0 :right 0
                     :white-space "pre-wrap"
                     :padding-left "1em"))

(define-class doc-mode () ()
  (:documentation "Enable doc-node style on HTML body.

This is suitable for whitespace-sensitive editing."))

(defsheet doc-mode `(("body" :inherit doc-node)))

(defsheet buffer
    `(("body" :inherit default)
      (".focus" :inherit focus)
      (".focus-tail" :inherit focus-tail)
      (".range-selection" :inherit range-selection)
      ("code" :inherit monospace)
      (":link" :inherit link)
      (".invisible" :display "none")
      ("::highlight(neomacs)" :inherit cursor)
      ("::highlight(neomacs-range)" :inherit range-selection)
      ("#neomacs-cursor" :inherit cursor)
      ("::-webkit-scrollbar" :display "none")))
