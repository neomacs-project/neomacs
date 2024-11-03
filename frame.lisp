(in-package #:neomacs)

(defun make-window-decoration (buffer)
  (check-displayable buffer)
  (setf (window-decoration buffer)
        (window-decoration-aux buffer)))

(defmacro with-delay-frame-update-views (&body body)
  `(call-with-delay-frame-update-views (lambda () ,@body)))

(defun update-window-decoration (buffer)
  "Recompute and apply BUFFER's window decoration."
  (when-let (frame-root (frame-root buffer))
    (with-current-buffer frame-root
      ;; `replace-node' first inserts the new node then deletes the
      ;; old node, which interacts badly with Electron's view handling
      ;; logic. If we add/remove views inside on-node-setup/cleanup,
      ;; if a buffer's content node appear in both the new and old
      ;; node, the buffer's view is first added then removed, which
      ;; cause it to eventually be removed (which should have been
      ;; retained, as in the DOM tree).

      ;; Therefore, we surround it with `with-delay-frame-update-views',
      ;; which record view addition/removal and compute the delta, only
      ;; adding/removing them once each when the block finishes.
      (with-delay-frame-update-views
        (let ((new (window-decoration-aux buffer)))
          (replace-node (window-decoration buffer) new)
          (setf (window-decoration buffer) new))))))

(define-mode echo-area-mode () ()
  (:documentation "Mode for echo area buffer."))

(defmethod window-decoration-aux ((buffer echo-area-mode))
  (dom `((:div :class "minibuffer")
         ((:div :class "content" :buffer ,(id buffer))))))

(define-mode frame-root-mode ()
  ((echo-area :initform (make-buffer " *echo-area*"
                                     :modes 'echo-area-mode
                                     :styles nil))
   (buffer-list :initform nil))
  (:documentation "Mode for frame root buffer."))

(defmethod buffer-list :around ((buffer frame-root-mode))
  (remove-if-not #'buffer-alive-p (call-next-method)))

(defun all-buffer-list (frame-root)
  (if frame-root
      (delete-duplicates
       (append (buffer-list frame-root)
               (alex:hash-table-values *buffer-table*))
       :from-end t)
      (alex:hash-table-values *buffer-table*)))

(defmethod on-buffer-loaded progn
    ((buffer frame-root-mode) (url t) (err t))
  (redisplay-windows buffer))

(defmethod on-post-command progn ((buffer frame-root-mode))
  (redisplay-windows buffer)
  ;; Update focus
  (when-let (window-node (node-after (focus)))
    (when-let (buffer (window-buffer window-node))
      (evaluate-javascript
       (ps:ps (ps:chain (js-buffer buffer) web-contents (focus)))
       nil))))

(ps:defpsmacro js-frame (buffer)
  `(ps:getprop (ps:chain -ceramic frames) (ps:lisp (id ,buffer))))

(ps:defpsmacro js-buffer (buffer)
  `(ps:getprop (ps:chain -ceramic buffers) (ps:lisp (id ,buffer))))

(defmethod enable-aux ((mode (eql 'frame-root-mode)))
  (evaluate-javascript
   (ps:ps
     (let ((frame (ps:chain -ceramic (create-frame (ps:lisp (id (current-buffer))) (ps:create))))
           (root (js-buffer (current-buffer))))
       (ps:chain frame content-view (add-child-view root))
       (let ((resize
               (lambda ()
                 (let ((bounds (ps:chain frame (get-content-bounds))))
                   (ps:chain root (set-bounds
                                   (ps:create :x 0 :y 0
                                              :width (ps:chain bounds width)
                                              :height (ps:chain bounds height)))))
                 (ps:chain root web-contents
                           (execute-java-script
                            (ps:lisp
                             (ps:ps
                               (let ((result (ps:create)))
                                 (dolist (c (ps:chain document (get-elements-by-class-name "content")))
                                   (let ((rect (ps:chain c (get-bounding-client-rect)))))
                                   (setf (ps:getprop result (ps:chain c (get-attribute "buffer")))
                                         (ps:create :x (ps:chain rect :x)
                                                    :y (ps:chain rect :y)
                                                    :width (ps:chain rect :width)
                                                    :height (ps:chain rect :height))))
                                 result))))
                           (then (lambda (result)
                                   (ps:for-in
                                    (buffer result)
                                    (let ((view (ps:getprop (ps:chain -ceramic buffers) buffer)))
                                      (when view
                                        (ps:chain view (set-bounds (ps:getprop result buffer)))))))))))))
       (ps:chain frame (set-title "Neomacs"))
       (ps:chain frame (set-menu nil))
       (ps:chain frame (on "resize" resize))
       (ps:chain frame (on "maximize" resize))))
   nil))

(defun cleanup-buffer-display (buffer)
  "Make sure BUFFER is not displayed in any frame.

This buries the buffer if BUFFER is a main buffer, or remove the view
if BUFFER is a child buffer. This function is called by
`delete-buffer' before deleting a buffer.

The logic to remove the view if BUFFER is a child buffer is to
workaround an Electron bug (as of Electron 33.0.2), which crashes if
view operations are applied to WebContentsView whose webContents has
been closed (`delete-buffer' closes the webContents). If this bug is
fixed in future Electron, our logic may be simplified."
  (when-let (frame-root (frame-root buffer))
    (if (window-decoration buffer)
        (bury-buffer buffer)
        (evaluate-javascript
         (ps:ps (ps:chain (js-frame frame-root) content-view
                          (remove-child-view (js-buffer buffer))))
         nil))))

(defmethod on-delete-buffer progn ((buffer frame-root-mode))
  (evaluate-javascript
   (ps:ps (ps:chain -ceramic (close-frame (ps:lisp (id (current-buffer))))))
   nil)
  (delete-buffer (echo-area buffer)))

(defmethod selectable-p-aux ((buffer frame-root-mode) pos)
  (let ((node (node-after pos)))
    (and (class-p node "buffer" "minibuffer")
         (attribute node "selectable"))))

(defun focused-buffer (&optional (frame-root (current-frame-root)))
  (window-buffer (node-after (focus frame-root))))

(defun current-buffer ()
  (or *current-buffer* (focused-buffer)))

(defun init-frame-root (init-buffer)
  (let* ((root (document-root (current-buffer)))
         (split (make-element "div" :class "horizontal")))
    (insert-nodes (end-pos root) split)
    (insert-nodes (end-pos split)
                  (make-window-decoration
                   (echo-area (current-buffer))))
    (insert-nodes (pos-down split)
                  (make-window-decoration
                   init-buffer))
    (setf (pos (focus)) (pos-down split))))

(defun make-frame-root (init-buffer)
  (lret ((buffer (make-buffer " *frame-root*" :styles nil)))
    (with-current-buffer buffer
      ;; FIXME: If I move (enable 'frame-root-mode) into (make-buffer
      ;; ... :modes 'frame-root-mode), it stops working.
      ;; Figure out why.
      (enable 'frame-root-mode)
      (init-frame-root init-buffer))))

(define-command other-window ()
  "Focus another buffer in cyclic order in current frame."
  (with-current-buffer (current-frame-root)
    (forward-node-cycle)))

(define-command prev-other-window ()
  "Focus another buffer in reverse cyclic order in current frame."
  (with-current-buffer (current-frame-root)
    (backward-node-cycle)))

(define-command delete-other-windows
    (&optional (buffer (current-buffer)))
  "Make BUFFER fill its frame."
  (check-displayed buffer)
  (with-current-buffer (current-frame-root)
    (erase-buffer)
    (init-frame-root buffer)))

(defvar *delay-frame-update-views* nil)

(defvar *frame-add-views* nil)

(defvar *frame-remove-views* nil)

(defun add-view (id)
  (when-let (buffer (gethash (parse-integer id) *buffer-table*))
    (evaluate-javascript
     (ps:ps (ps:chain (js-frame (current-buffer)) content-view
                      (add-child-view (ps:getprop (ps:chain -ceramic buffers)
                                                  (ps:lisp id)))))
     nil)
    (setf (frame-root buffer) (current-buffer))))

(defun remove-view (id)
  (when-let (buffer (gethash (parse-integer id) *buffer-table*))
    (evaluate-javascript
     (ps:ps (ps:chain (js-frame (current-buffer)) content-view
                      (remove-child-view (ps:getprop (ps:chain -ceramic buffers)
                                                     (ps:lisp id)))))
     nil)
    (setf (window-decoration buffer) nil
          (frame-root buffer) nil)))

(defun call-with-delay-frame-update-views (thunk)
  (let ((*delay-frame-update-views* t)
        *frame-add-views*
        *frame-remove-views*)
    (multiple-value-prog1 (funcall thunk)
      (mapc #'remove-view *frame-remove-views*)
      (mapc #'add-view *frame-add-views*))))

(defmethod on-node-setup progn ((buffer frame-root-mode) node)
  (when (class-p node "content")
    (when-let (id (attribute node "buffer"))
      (if *delay-frame-update-views*
          (if (member id *frame-remove-views* :test 'equal)
              (alex:deletef *frame-remove-views* id)
              (push id *frame-add-views*))
          (add-view id))
      (when (class-p node "main")
        (let ((buf (gethash (parse-integer id) *buffer-table*)))
          (setf (buffer-list buffer)
                (cons buf (remove buf (buffer-list buffer))))))))
  (when (class-p node "vertical" "horizontal")
    (with-post-command (node 'first-child 'last-child)
      (when (eql (first-child node) (last-child node))
        (splice-node node)))))

(defmethod on-node-cleanup progn ((buffer frame-root-mode) node)
  (when (class-p node "content")
    (when-let (id (attribute node "buffer"))
      (if *delay-frame-update-views*
          (if (member id *frame-add-views* :test 'equal)
              (alex:deletef *frame-add-views* id)
              (push id *frame-remove-views*))
          (remove-view id)))))

(defun check-displayable (buffer)
  "Signal error if BUFFER is not suitable for display."
  (unless (buffer-alive-p buffer)
    (error "~A is deleted" buffer))
  (when (typep buffer 'frame-root-mode)
    (error "Cannot display frame root ~A" buffer))
  (when (frame-root buffer)
    (error "~A is already displayed" buffer)))

(defun check-displayed (buffer)
  "Signal error if BUFFER is not already displayed."
  (unless (frame-root buffer)
    (error "~A is not displayed" buffer)))

(define-command switch-to-buffer
    (&optional
     (buffer
      (get-buffer
       (completing-read
        "Switch to buffer: " 'buffer-list-mode
        :exclude-buffers (list (current-buffer)))))
     (victim (focused-buffer)))
  (check-displayed victim)
  (check-displayable buffer)
  (with-current-buffer (frame-root victim)
    (insert-nodes (pos-right (window-decoration victim))
                  (make-window-decoration buffer))
    (close-buffer-display victim))
  buffer)

(defun make-scratch ()
  (lret ((buffer (make-buffer "*scratch*" :modes '(lisp-mode file-mode))))
    (with-current-buffer buffer
      (setf (file-path buffer)
            (asdf:system-relative-pathname :neomacs #p"scratch.lisp"))
      (revert-buffer)
      (disable 'file-mode))))

(defun replacement-buffer (&optional (buffer (current-buffer)))
  "Find a buffer to display in place of BUFFER.

A replacement buffer has to be alive and not already displayed."
  (let (replacement)
    ;; Try to find any buffer for replacement.
    (iter (for buf in (all-buffer-list (frame-root buffer)))
      (unless (or (frame-root buf)
                  (typep buf 'frame-root-mode))
        (setq replacement buf)
        (return)))
    (or replacement (make-scratch))))

(define-command bury-buffer (&optional (buffer (current-buffer)))
  "Stop displaying BUFFER."
  (check-displayed buffer)
  (switch-to-buffer (replacement-buffer buffer) buffer))

(define-command close-buffer-display (&optional (buffer (current-buffer)))
  "Close the window which displays BUFFER.

If BUFFER is the only displayed buffer in a frame, this functions do
nothing instead, because deleting it would break window management."
  (check-displayed buffer)
  (with-current-buffer (frame-root buffer)
    (with-marker (m (window-decoration buffer))
      (forward-node-cycle m)
      (when (eq (node-after m) (window-decoration buffer))
        (message "Attempting to delete sole window in a frame")
        (return-from close-buffer-display)))
    (delete-node (window-decoration buffer))))

(define-command quit-buffer (&optional (buffer (current-buffer)))
  "Delete BUFFER and close its display, if any."
  (when (frame-root buffer)
    (close-buffer-display buffer))
  (delete-buffer buffer))

(define-command display-buffer-right (&optional (buffer (replacement-buffer)))
  "Split a window to the right and display BUFFER in it."
  (check-displayable buffer)
  (with-current-buffer (current-frame-root)
    (unless (class-p (node-containing (focus)) "vertical")
      (wrap-node (focus) (make-element "div" :class "vertical")))
    (insert-nodes (pos-right (focus))
                  (make-window-decoration buffer))
    buffer))

(define-command display-buffer-below (&optional (buffer (replacement-buffer)))
  "Split a window to the bottom and display BUFFER in it."
  (check-displayable buffer)
  (with-current-buffer (current-frame-root)
    (unless (class-p (node-containing (focus)) "horizontal")
      (wrap-node (focus) (make-element "div" :class "horizontal")))
    (insert-nodes (pos-right (focus))
                  (make-window-decoration buffer))
    buffer))

(defun focus-buffer (buffer)
  "Give BUFFER focus.

BUFFER must be already displayed."
  (check-displayed buffer)
  (with-current-buffer (frame-root buffer)
    (setf (pos (focus)) (window-decoration buffer))
    buffer))

(define-keys global
  "C-x o" 'other-window
  "C-x 0" 'close-buffer-display
  "C-x 1" 'delete-other-windows
  "C-x 2" 'display-buffer-below
  "C-x 3" 'display-buffer-right
  "C-x b" 'switch-to-buffer
  "C-x k" 'delete-buffer)

(defun redisplay-windows
    (&optional (frame-root (current-frame-root)))
  "Ask FRAME-ROOT to update its windows."
  (evaluate-javascript
   (ps:ps (ps:chain (js-frame frame-root) (emit "resize")))
   nil))

(defvar *current-frame-root*)

(defun current-frame-root ()
  *current-frame-root*)

(defun content-node-buffer (node)
  (gethash (parse-integer (attribute node "buffer")) *buffer-table*))

(defun window-buffer (window-node)
  (content-node-buffer
   (only-elt (get-elements-by-class-name window-node "main"))))

(defvar *message-log-max* 1000
  "Maximum number of lines to keep in the `*Messages*' buffer.

If nil, disable message logging. If t, log messages but don't truncate
`*Messages*' buffer.")

(defun get-message-buffer ()
  (get-buffer-create "*Messages*" :modes '(read-only-mode doc-mode)))

(defun truncate-node (node n)
  (iter
    (with i = 1)
    (for pos first (end-pos node)
         then (npos-left pos))
    (while pos)
    (when (line-start-p pos)
      (incf i))
    (unless (< i n)
      (delete-nodes (pos-down node) pos)
      (return))))

(defun message (control-string &rest format-arguments)
  "Log a message in `*Messages*' buffer."
  (with-current-buffer (echo-area *current-frame-root*)
    (erase-buffer)
    (when control-string
      (let ((message (apply #'format nil control-string format-arguments)))
        (insert-nodes (end-pos (document-root (current-buffer))) message)
        (when *message-log-max*
          (with-current-buffer (get-message-buffer)
            (let ((*inhibit-read-only* t))
              (unless (eql *message-log-max* t)
                (truncate-node (document-root (current-buffer)) *message-log-max*))
              (insert-nodes (end-pos (document-root (current-buffer)))
                            message
                            (make-new-line-node)))))))))

(define-command open-dev-tools ()
  (evaluate-javascript
   (ps:ps (ps:chain (js-buffer (current-buffer)) web-contents (open-dev-tools)))
   nil))

(defstyle frame-root-mode
    `((".vertical"
       :flex "1 0 1em" :display "flex" :flex-flow "row"
       :gap "24px" :width "100%" :height "100%")
      (".horizontal"
       :flex "1 0 1em" :display "flex" :flex-flow "column"
       :gap "24px" :height "100%" :width "100%")
      (".content" :flex "1 0 1em" :position "relative")
      (".vertical-child-container"
       :flex "1 0 1em"
       :display "flex" :flex-flow "row")
      (".buffer" :flex "1 0 1em"
                 :display "flex" :flex-flow "column"
                 :backdrop-filter "blur(10px)")
      (".minibuffer"
       :flex "0 0 2em"
       :display "flex" :flex-flow "column"
       :backdrop-filter "blur(10px)")
      (".header" :inherit header)
      (".focus .header" :inherit header-focus)
      (".header-buffer-name" :inherit header-buffer-name)
      (".header-buffer-modes" :inherit header-buffer-modes)
      ("body"
       :padding "32px"
       :margin 0
       :inherit default
       :background-size "cover"
       :background-image "url(https://sozaino.site/wp-content/uploads/2021/08/sf35.png)")))

(defstyle echo-area-mode
    `(("body" :inherit default
              :margin-top 0
              :margin-bottom 0
              :white-space "pre-wrap")))

(defstyle header `(:padding "8px"
                   :display "flex" :flex-flow "row"
                   :margin-bottom "8px"
                   :background-color "rgba(169,151,160,0.2)"))

(defstyle header-buffer-name `(:flex "1 0 1em"))

(defstyle header-buffer-modes
    `(:flex "1 0 1em"
      :text-align "right"
      :white-space "nowrap"
      :text-overflow "ellipsis"))

(defstyle header-focus `(:background-color "rgba(169,151,160,0.4)"))
