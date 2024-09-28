(in-package :neomacs)

;;; Neomacs Mode

(defun combine-around-hook (hook cont &rest args)
  (labels ((process (handlers-alist)
             (if handlers-alist
                 (if (cdar handlers-alist)
                     (apply (hooks:fn (caar handlers-alist))
                            (lambda () (process (cdr handlers-alist)))
                            args)
                     (process (cdr handlers-alist)))
                 (funcall cont))))
    (process (hooks:handlers-alist hook))))

(defvar *adjust-marker-direction* 'forward)

(defun self-insert-command (string)
  (nyxt:lambda-command self-insert ()
    (undo-auto-amalgamate)
    (hooks:run-hook (self-insert-hook (find-submode 'neomacs-mode))
                    (focus) string)))

(define-mode neomacs-mode ()
  "Neomacs mode."
  ((selectable-p-hook
    (make-instance 'hooks:hook-any :combination #'combine-around-hook)
    :type hooks:hook)
   (word-boundary-list (list #\ ) :type (list-of character))
   (post-command-hook (make-instance 'hooks:hook-any) :type hooks:hook)
   (pre-command-hook (make-instance 'hooks:hook-any) :type hooks:hook)
   (self-insert-hook
    (make-instance 'hooks:hook-any
                   :combination #'hooks:combine-hook-until-success)
    :type hooks:hook)
   (node-setup-hook (make-instance 'hooks:hook-any)
                    :type hooks:hook)
   (focus-move-hook (make-instance 'hooks:hook-any
                                   :handlers '(redisplay-focus)))
   (completion-hook (make-instance 'hooks:hook-any
                                   :combination #'hooks:combine-hook-until-success))
   (focus-marker)
   (selection-marker)
   (markers nil :type list)
   (restriction nil)
   (next-nyxt-id 0 :accessor next-nyxt-id :type integer)
   (lock (bt:make-lock))
   (read-only-p nil :type boolean)
   (this-command)
   (last-command)
   (scroll-margin
    0.2
    :type (real 0 0.5)
    :documentation "With value s, try to keep cursor within [s,1-s] portion of the viewport.")
   (scroll-lines 10 :type (integer 1))
   (styles (list 'neomacs-mode 'completion) :type (list-of symbol))

   (keyscheme-map
    (lret ((m (keymaps:define-keyscheme-map "neomacs" ()
                keyscheme:default
                (list "backspace" 'backward-delete
                      "space" (self-insert-command " ")
                      "hyphen" (self-insert-command "-")
                      "return" 'new-line
                      "right" 'forward-node
                      "left" 'backward-node
                      "M-right" 'forward-word
                      "M-left" 'backward-word
                      "down" 'next-line
                      "up" 'previous-line
                      "end" 'end-of-line
                      "home" 'beginning-of-line)
                keyscheme:emacs
                '("C-f" forward-node
                  "C-b" backward-node
                  "M-f" forward-word
                  "M-b" backward-word
                  "C-M-f" forward-element
                  "C-M-b" backward-element
                  "M-<" beginning-of-buffer
                  "M->" end-of-buffer
                  "C-a" beginning-of-line
                  "C-e" end-of-line
                  "M-a" beginning-of-defun
                  "M-e" end-of-defun
                  "C-n" next-line
                  "C-p" previous-line
                  "C-v" scroll-down-command
                  "M-v" scroll-up-command

                  "M-backspace" backward-cut-word
                  "C-d" forward-delete
                  "M-d" forward-cut-word
                  "C-w" cut-element
                  "M-w" copy-element
                  "C-y" paste
                  "M-y" paste-pop
                  "C-k" forward-cut

                  "C-s" isearch))))
      (iter (for i from 32 below 127)
        (for char = (code-char i))
        (unless (member char '(#\  #\-))
          (keymaps:define-key (gethash keyscheme:default m) (string char)
            (self-insert-command (string char)))))))
   (rememberable-p nil))
  (:toggler-command-p nil))

(defun current-neomacs (&optional (buffer (current-buffer)))
  (or (find-submode 'neomacs-mode buffer)
      (progn
        (enable-modes* 'neomacs-mode buffer)
        (find-submode 'neomacs-mode buffer))))

(defun focus (&optional (neomacs (current-neomacs)))
  (focus-marker neomacs))

(defvar *inside-dom-update-p* nil)

(defvar *dom-update-stream* nil)

(defun call-with-dom-update (neomacs thunk)
  (if *inside-dom-update-p*
      (funcall thunk)
      (bt:with-lock-held ((lock neomacs))
        (let* ((*dom-update-stream* (make-string-output-stream))
               (*inside-dom-update-p* t))
          (unwind-protect
               (with-delayed-evaluation
                 (funcall thunk))
            (render-focus (pos (focus neomacs)))
            (let ((s (get-output-stream-string *dom-update-stream*)))
              (when (> (length s) 0)
                (ffi-buffer-evaluate-javascript
                 (buffer neomacs)
                 s))))))))

(defmacro with-dom-update (neomacs &body body)
  `(call-with-dom-update ,neomacs (lambda () ,@body)))

(defun dispatch-command-sync (function)
  "Run FUNCTION synchronously."
  (echo-dismiss)
  (let ((ignored-commands '(execute-command
                            execute-predicted-command
                            next-suggestion
                            previous-suggestion
                            next-source
                            previous-source))
        (function-function (typecase function
                             (symbol (symbol-function function))
                             (function function))))
    (unless (find (name function-function)
                  ignored-commands
                  :test (lambda (x y) (search (symbol-name x) (symbol-name y))))
      (analysis:add-record (command-model *browser*)
                           (list (last-command *browser*) function))
      (setf (last-command *browser*) function-function))
    (with-protect ("Error: ~a" :condition)
      (with-current-buffer (current-buffer)
        (if-let (n (find-submode 'neomacs-mode))
          (let ((*adjust-marker-direction* *adjust-marker-direction*))
            (setf (last-command n) (this-command n)
                  (this-command n) function-function)
            (hooks:run-hook (pre-command-hook n))
            (unwind-protect
                 (with-dom-update n
                   (funcall function)
                   (case *adjust-marker-direction*
                     ((forward) (ensure-selectable (focus n)))
                     ((backward) (ensure-selectable (focus n) t))))
              (hooks:run-hook (post-command-hook n))))
          (funcall function))))
    t))

(defmethod nyxt::window-delete-panel-buffer ((window window) (buffer panel-buffer))
  "Remove a panel buffer from a window."
  (setf (nyxt::panel-buffers window)
        (remove buffer (nyxt::panel-buffers window)))
  (hooks:run-hook (buffer-delete-hook buffer) buffer)
  (ffi-window-delete-panel-buffer window buffer))

(defun neomacs-buffer-loaded-hook (buffer)
  (let ((marker (focus (find-submode 'neomacs-mode buffer)))
        (mode (find-submode 'neomacs-mode buffer)))
    (setf (pos marker) (pos marker))
    (dolist (style (styles mode))
      (update-style mode style))))

(defun neomacs-buffer-delete-hook (buffer)
  (let ((mode (find-submode 'neomacs-mode buffer)))
    (dolist (style (styles mode))
      (remove-observer (css-cell style) mode
                       :key (lambda (f) (and (typep f 'update-neomacs)
                                             (slot-value f 'mode)))))))

(defmethod enable ((mode neomacs-mode) &key)
  (hooks:add-hook (dispatch-command-hook (buffer mode)) 'dispatch-command-sync)
  (hooks:add-hook (buffer-loaded-hook (buffer mode))
                  'neomacs-buffer-loaded-hook)
  (hooks:add-hook (buffer-delete-hook (buffer mode))
                  'neomacs-buffer-delete-hook)
  (dolist (style (styles mode))
    (add-observer (css-cell style)
                  (nclo update-neomacs (cell)
                    (declare (ignore cell))
                    (update-style mode style)))))

(defmethod disable ((mode neomacs-mode) &key)
  (hooks:remove-hook (dispatch-command-hook (buffer mode)) 'dispatch-command-sync)
  (hooks:remove-hook (buffer-loaded-hook (buffer mode))
                     'neomacs-buffer-loaded-hook)
  (neomacs-buffer-delete-hook (buffer mode))
  (hooks:remove-hook (buffer-delete-hook (buffer mode))
                     'neomacs-buffer-delete-hook))

;;; Parenscript utils

(defun assign-neomacs-id (node)
  (setf (attribute node "nyxt-identifier")
        (princ-to-string (incf (next-nyxt-id (find-submode 'neomacs-mode)))))
  node)

(ps:defpsmacro js-node (node)
  (cond ((text-node-p node)
         (if-let (next (next-sibling node))
           `(ps:chain (js-node ,next) previous-sibling)
           `(ps:chain (js-node ,(parent node)) last-child)))
        ((element-p node)
         `(ps:chain document (query-selector ,(format nil "[nyxt-identifier='~a']"
                                                      (attribute node "nyxt-identifier")))))
        ((null node) nil)
        (t (error "Unknown node type ~a." node))))

(defun get-bounding-client-rect (node)
  (ffi-buffer-evaluate-javascript
   (buffer (host node))
   (let (ps:*parenscript-stream*)
     (ps:ps*
      `(let ((r (ps:chain (js-node ,node) (get-bounding-client-rect))))
         (list (ps:chain r x) (ps:chain r y)
               (ps:chain r width) (ps:chain r height)))))))

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

;;; Render focus

(defun render-focus (pos)
  (setq pos (resolve-marker pos))
  (send-dom-update
   (bind ((node (node-after pos))
          (parent (node-containing pos))
          (scroll-margin (scroll-margin (host pos))))
     `(let (anchor-x anchor-y)
        ,(if (or (characterp node)
                 (and node (equal (tag-name node) "br")))
             `(let ((range (ps:new (-range))))
                (dolist (e (ps:chain document (get-elements-by-class-name "focus")))
                  (ps:chain e class-list (remove "focus")))
                (dolist (e (ps:chain document (get-elements-by-class-name "focus-tail")))
                  (ps:chain e class-list (remove "focus-tail")))
                ,(if (characterp node)
                     `(let* ((text-node (js-node ,(text-pos-node pos)))
                             (parent (ps:chain text-node parent-node)))
                        (ps:chain range (set-start text-node
                                                   ,(text-pos-offset pos)))
                        (ps:chain range (set-end text-node
                                                 ,(1+ (text-pos-offset pos)))))
                     `(let ((element (js-node ,node)))
                        (ps:chain range (select-node element))))
                (let ((rect (ps:chain range (get-bounding-client-rect)))
                      (highlight (ps:chain document (get-element-by-id "neomacs-highlight"))))
                  (setq anchor-x (ps:chain rect x))
                  (setq anchor-y (ps:chain rect y))
                  (if (> (ps:chain rect width) 5)
                      (progn
                        (setf (ps:chain highlight style display) "none")
                        (let ((highlight (ps:chain -c-s-s highlights (get "neomacs"))))
                          (unless highlight
                            (setq highlight (ps:new (-highlight)))
                            (ps:chain -c-s-s highlights (set "neomacs" highlight)))
                          (ps:chain highlight (clear))
                          (ps:chain highlight (add range))))
                      (progn
                        (let ((highlight (ps:chain -c-s-s highlights (get "neomacs"))))
                          (when highlight (ps:chain highlight (clear))))
                        (setf (ps:chain highlight style display) "inline")
                        (setf (ps:chain highlight style left)
                              (+ (ps:chain rect x) (ps:chain window scroll-x) "px")
                              (ps:chain highlight style top)
                              (+ (ps:chain rect y) (ps:chain window scroll-y) "px")
                              (ps:chain highlight style width)
                              (+ (ps:chain rect width) "px")
                              (ps:chain highlight style height)
                              (+ (ps:chain rect height) "px")))))
                nil)
             `(let ((element (js-node ,(or node parent)))
                    (highlight (ps:chain document (get-element-by-id "neomacs-highlight"))))
                (dolist (e (ps:chain document (get-elements-by-class-name "focus")))
                  (ps:chain e class-list (remove "focus")))
                (dolist (e (ps:chain document (get-elements-by-class-name "focus-tail")))
                  (ps:chain e class-list (remove "focus-tail")))
                (setf (ps:chain highlight style display) "none")
                (let ((highlight (ps:chain -c-s-s highlights (get "neomacs"))))
                  (when highlight (ps:chain highlight (clear))))
                (ps:chain element class-list
                          (add ,(if node "focus" "focus-tail")))
                (let ((rect (ps:chain element (get-bounding-client-rect))))
                  (setq anchor-x (ps:chain rect ,(if node 'left 'right)))
                  (setq anchor-y (ps:chain rect ,(if node 'top 'bottom))))))
        (ps:chain window
                  (scroll-to
                   (+ (ps:chain window scroll-x)
                      (min (- anchor-x (* ,scroll-margin (ps:chain window inner-width))) 0)
                      (max (- anchor-x (* ,(- 1 scroll-margin) (ps:chain window inner-width))) 0))
                   (+ (ps:chain window scroll-y)
                      (min (- anchor-y (* ,scroll-margin (ps:chain window inner-height))) 0)
                      (max (- anchor-y (* ,(- 1 scroll-margin) (ps:chain window inner-height))) 0))))))))

(defun redisplay-focus (saved pos)
  (declare (ignore saved))
  (unless *inside-dom-update-p*
    (with-current-buffer (buffer (host pos))
      (render-focus pos))))

;;; Read-only state

(define-condition read-only-error (error)
  ((buffer :initarg :buffer))
  (:report "~a is read only." buffer))

(defvar *inhibit-read-only* nil)

(defun check-read-only (neomacs)
  (unless *inhibit-read-only*
    (restart-case
        (when (read-only-p neomacs)
          (error 'read-only-error :buffer (buffer neomacs)))
      (continue ()
        :report "Go on write to this buffer.")
      (continue* ()
        :report "Make buffer writable and go on."
        (setf (read-only-p neomacs) nil)))))

;;; Styles

(defun update-style (neomacs style)
  (let ((id (format nil "neomacs-style-~a" style)))
    (with-current-buffer (buffer neomacs)
      (send-dom-update
       `(let ((element (ps:chain document (get-element-by-id ,id))))
          (unless element
            (setq element (ps:chain document (create-element "style")))
            (setf (ps:chain element id) ,id)
            (ps:chain document head (append-child element)))
          (setf (ps:chain element inner-h-t-m-l)
                ,(cell-ref (css-cell style))))))))

(defun remove-style (neomacs style)
  (let ((id (format nil "neomacs-style-~a" style)))
    (with-current-buffer (buffer neomacs)
      (send-dom-update
       `(let ((element (ps:chain document (get-element-by-id ,id))))
          (when element
            (ps:chain element (remove))))))))

(defmethod (setf styles) (new-val (mode neomacs-mode))
  (dolist (style (set-difference new-val (slot-value mode 'styles)))
    (update-style mode style)
    (add-observer (css-cell style)
                  (nclo update-neomacs (cell)
                    (declare (ignore cell))
                    (update-style mode style))))
  (dolist (style (set-difference (slot-value mode 'styles) new-val))
    (remove-style mode style)
    (remove-observer (css-cell style) mode
                     :key (lambda (f) (and (typep f 'update-neomacs)
                                           (slot-value f 'mode)))))
  (setf (slot-value mode 'styles) new-val))

(defstyle default `(:font-family "Verdana"))
(defstyle focus `(:background-color "#f0f7ff"))
(defstyle selection `(:background-color "#bde1ff"))
(defstyle keyword `(:color "#fd79a8"))
(defstyle macro `(:color "#fd79a8"))
(defstyle special-operator `(:color "#fd79a8"))
(defstyle string `(:color "#a29bfe"))
(defstyle comment `(:color "#777"))
(defstyle focus-tail
    `(((:append "::after") :content " " :inherit selection)
      :inherit focus))
(defstyle cursor
    `(:position "absolute"
      :min-height "1em"
      :min-width "0.3em"
      :z-index "-1"
      :inherit selection))

(defstyle doc-node `(((:append ".focus-tail::after")
                      :content "  "
                      :white-space "pre"
                      :inherit selection)
                     :left 0 :right 0
                     :white-space "pre-wrap"
                     :padding-left "1em"))

(defstyle neomacs-mode
    `(("body" :inherit default)
      (".focus" :inherit focus)
      (".focus-tail" :inherit focus-tail)
      ("::highlight(neomacs)" :inherit cursor)
      ("#neomacs-highlight" :inherit cursor)
      (".doc" :inherit doc-node)))
