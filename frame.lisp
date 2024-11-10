(in-package #:neomacs)

(sera:export-always
    '(focused-buffer current-frame-root
      replacement-buffer
      get-message-buffer message))

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
      (with-amalgamate-js frame-root
        (with-delay-frame-update-views
         (let ((new (window-decoration-aux buffer)))
           (replace-node (window-decoration buffer) new)
           (setf (window-decoration buffer) new)))))))

(define-mode echo-area-mode () ()
  (:documentation "Mode for echo area buffer."))

(defmethod window-decoration-aux ((buffer echo-area-mode))
  (dom `(:div :class "minibuffer"
              (:div :class "content" :buffer ,(id buffer)))))

(define-mode frame-root-mode ()
  ((echo-area :initform (make-buffer " *echo-area*"
                                     :mode 'echo-area-mode
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
       :global))))

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
                               (ps:new (-promise
                                        (lambda (resolve)
                                          (set-timeout
                                           (lambda ()
                                             (let ((result (ps:create)))
                                               (dolist (c (ps:chain document (get-elements-by-class-name "content")))
                                                 (let ((rect (ps:chain c (get-bounding-client-rect)))))
                                                 (setf (ps:getprop result (ps:chain c (get-attribute "buffer")))
                                                       (ps:create :x (ps:chain rect :x)
                                                                  :y (ps:chain rect :y)
                                                                  :width (ps:chain rect :width)
                                                                  :height (ps:chain rect :height))))
                                               (funcall resolve result)))
                                           0)))))))
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
   :global))

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
         :global))))

(defmethod on-delete-buffer progn ((buffer frame-root-mode))
  (evaluate-javascript
   (ps:ps (ps:chain -ceramic (close-frame (ps:lisp (id (current-buffer))))))
   :global)
  (delete-buffer (echo-area buffer)))

(defmethod selectable-p-aux ((buffer frame-root-mode) pos)
  (let ((node (node-after pos)))
    (and (class-p node "buffer" "minibuffer")
         (attribute node "selectable"))))

(defun focused-buffer (&optional (frame-root (current-frame-root)))
  (when frame-root
    (window-buffer (node-after (focus frame-root)))))

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
      ;; ... :mode 'frame-root-mode), it stops working.
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
     :global)
    (setf (frame-root buffer) (current-buffer))))

(defun remove-view (id)
  (when-let (buffer (gethash (parse-integer id) *buffer-table*))
    (evaluate-javascript
     (ps:ps (ps:chain (js-frame (current-buffer)) content-view
                      (remove-child-view (ps:getprop (ps:chain -ceramic buffers)
                                                     (ps:lisp id)))))
     :global)
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
      (setf (window-min-height buffer) nil
            (window-min-width buffer) nil)
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
  :interactive
  (lambda ()
    (list
     (get-buffer
      (completing-read
       "Switch to buffer: " 'buffer-list-mode
       :exclude-buffers (list (current-buffer))))))
  (buffer &optional (victim (focused-buffer)))
  (if (frame-root buffer)
      (focus-buffer buffer)
      (progn
        (check-displayed victim)
        (check-displayable buffer)
        (with-current-buffer (frame-root victim)
          (insert-nodes (pos-right (window-decoration victim))
                        (make-window-decoration buffer))
          (close-window victim))
        buffer)))

(defun make-scratch ()
  (lret ((buffer (make-buffer
                  "*scratch*" :mode '(lisp-mode file-mode)
                  :file-path (asdf:system-relative-pathname :neomacs #p"scratch.lisp"))))
    (with-current-buffer buffer
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

(define-command close-window (&optional (buffer (current-buffer)))
  "Close the window which displays BUFFER.

If BUFFER is the only displayed buffer in a frame, this functions do
nothing instead, because deleting it would break window management."
  (check-displayed buffer)
  (with-current-buffer (frame-root buffer)
    (with-marker (m (window-decoration buffer))
      (forward-node-cycle m)
      (when (eq (node-after m) (window-decoration buffer))
        (message "Attempting to delete sole window in a frame")
        (return-from close-window)))
    (delete-node (window-decoration buffer))))

(define-command quit-buffer (&optional (buffer (current-buffer)))
  "Delete BUFFER and restore any `display-buffer' action to display it.

This runs BUFFER's `window-quit-action' to restore window
configuration. `display-buffer' action functions is responsible for
setting appropriate `window-quit-action'.  If `window-quit-action' is
nil, the window displaying BUFFER is closed instead."
  (when (frame-root buffer)
    (if-let (action (window-quit-action buffer))
      (funcall action buffer)
      (close-window buffer)))
  (delete-buffer buffer))

(define-command split-window-right (&optional (buffer (replacement-buffer)))
  "Split a window to the right and display BUFFER in it."
  (check-displayable buffer)
  (with-current-buffer (current-frame-root)
    (unless (class-p (node-containing (focus)) "vertical")
      (wrap-node (focus) (make-element "div" :class "vertical")))
    (insert-nodes (pos-right (focus))
                  (make-window-decoration buffer))
    buffer))

(define-command split-window-below (&optional (buffer (replacement-buffer)))
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

(define-keys :global
  "C-x o" 'other-window
  "C-x 0" 'close-window
  "C-x 1" 'delete-other-windows
  "C-x 2" 'split-window-below
  "C-x 3" 'split-window-right
  "C-x b" 'switch-to-buffer
  "C-x k" 'delete-buffer)

(defun redisplay-windows
    (&optional (frame-root (current-frame-root)))
  "Ask FRAME-ROOT to update its windows."
  (evaluate-javascript
   (ps:ps (ps:chain (js-frame frame-root) (emit "resize")))
   :global))

(defvar *current-frame-root* nil)

(defun current-frame-root ()
  *current-frame-root*)

(defun content-node-buffer (node)
  (gethash (parse-integer (attribute node "buffer")) *buffer-table*))

(defun window-buffer (window-node)
  (content-node-buffer
   (only-elt (get-elements-by-class-name window-node "main"))))

;;; Display buffer

(defvar *display-buffer-base-actions*
  '(display-buffer-pop-up-window
    display-buffer-use-some-window))

(defun display-buffer
    (buffer &optional (actions *display-buffer-base-actions*))
  "Display BUFFER in some window without focusing it."
  (unless (frame-root buffer)
    (with-current-buffer (current-frame-root)
      (some (alex:rcurry #'funcall buffer) actions))))

(defun pop-to-buffer
    (buffer &optional (actions *display-buffer-base-actions*))
  "Display BUFFER in some window and focus it.

This function wraps `window-quit-action' to restore focused buffer
before other quit actions."
  (display-buffer buffer actions)
  (setf (window-quit-action buffer)
        (let ((next (or (window-quit-action buffer)
                        #'close-window))
              (old-focus (focused-buffer)))
          (nclo quit-pop-to-buffer (buffer)
            (when (frame-root old-focus)
              (focus-buffer old-focus))
            (funcall next buffer))))
  (focus-buffer buffer))

(defun display-buffer-use-some-window (buffer)
  "Display BUFFER in an existing window."
  (with-marker (m (focus))
    (forward-node-cycle m)
    (let ((victim (window-buffer (node-after m))))
      (setf (window-quit-action buffer)
            (nclo quit-display-buffer-use-some-window (buffer)
              (switch-to-buffer victim buffer)))
      (switch-to-buffer buffer victim))))

(defvar *split-width-threshold* 800)

(defvar *split-height-threshold* 600)

(defun display-buffer-pop-up-window (buffer)
  "Display BUFFER by splitting the focused window."
  (bind (((_ _ w h)
          (get-bounding-client-rect (window-decoration
                                     (focused-buffer)))))
    (cond ((> w *split-width-threshold*)
           (split-window-right buffer))
          ((> h *split-height-threshold*)
           (split-window-below buffer)))))

;;; Messages and echo area

(defun get-message-buffer ()
  (get-buffer-create "*Messages*" :mode '(read-only-mode doc-mode)))

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

(defvar *window-layout-helper* nil)

(defun fit-buffer-size
    (buffer client-property decoration-property
     slot size)
  (check-displayed buffer)
  (let ((size
          (or size
              (evaluate-javascript-sync
               (format
                nil "document.documentElement.~a"
                client-property)
               buffer))))
    ;; Avoid set size if possible to reduce flickering
    (unless (eql (slot-value buffer slot) size)
      (with-current-buffer (frame-root buffer)
        (evaluate-javascript-sync
         (ps:ps
           (setf
            (ps:getprop
             (ps:chain (js-node-1 (window-decoration buffer))
                       style)
             (ps:lisp decoration-property))
            (ps:lisp (format nil "~apx" size)))
           nil)
         (frame-root buffer)))
      (setf (slot-value buffer slot) size))))

(defun fit-buffer-height (buffer &optional height)
  (run-in-helper
   '*window-layout-helper*
   'fit-buffer-size buffer "scrollHeight" "min-height"
   'window-min-height height))

(defun fit-buffer-width (buffer &optional width)
  (run-in-helper
   '*window-layout-helper*
   'fit-buffer-size buffer "scrollWidth" "min-width"
                   'window-min-width width))

(defun message (control-string &rest format-arguments)
  "Echo and log a message in `*Messages*' buffer.

If CONTROL-STRING is a string, format it with FORMAT-ARGUMENTS and
echo it.

CONTROL-STRING can also be a list of DOM nodes (`element's or
`text-node's), which are displayed and logged. FORMAT-ARGUMENTS must
be nil in this case."
  (if *current-frame-root*
      (with-current-buffer (echo-area *current-frame-root*)
        (erase-buffer)
        (if control-string
            (let ((message
                     (if (stringp control-string)
                         (list (apply #'format nil control-string format-arguments)
                               (make-new-line-node))
                         control-string)))
              (apply #'insert-nodes (end-pos (document-root (current-buffer))) message)
              (fit-buffer-height (current-buffer) nil)
              (when *message-log-max*
                (with-current-buffer (get-message-buffer)
                  (let ((*inhibit-read-only* t))
                    (unless (eql *message-log-max* t)
                      (truncate-node (document-root (current-buffer)) *message-log-max*))
                    (apply #'insert-nodes
                           (end-pos (document-root (current-buffer)))
                           (mapcar #'clone-node message))))))
            (fit-buffer-height (current-buffer) 0)))
      (when control-string
        (apply #'format *error-output* control-string format-arguments)
        (terpri))))

(define-command open-dev-tools ()
  (evaluate-javascript
   (ps:ps (ps:chain (js-buffer (current-buffer)) web-contents (open-dev-tools)))
   :global))

;;; Style

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
                 :min-width "1em"
                 :min-height "1em"
                 :display "flex" :flex-flow "column"
                 :backdrop-filter "blur(10px)")
      (".minibuffer"
       :flex "0 0 2em"
       :display "flex" :flex-flow "column"
       :backdrop-filter "blur(10px)")
      (".header" :inherit header)
      (".header > div" :padding-right "1em")
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

(defstyle header-buffer-name `(:flex "1 0 1em"
                               :white-space "nowrap"))

(defstyle header-buffer-modes
    `(:flex "1 0 1em"
      :text-align "right"
      :white-space "nowrap"
      :overflow "hidden"
      :text-overflow "ellipsis"))

(defstyle header-focus `(:background-color "rgba(169,151,160,0.4)"))
