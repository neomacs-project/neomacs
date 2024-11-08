(in-package #:neomacs)

(sera:export-always
    '(compute-completion
      completions replace-range completion-buffer))

(define-mode completion-list-mode (list-mode)
  ((completions :initform nil :initarg :completions)))

(defmethod generate-rows ((buffer completion-list-mode))
  (iter (for (completion annotation) in (completions buffer))
    (insert-nodes
     (focus)
     (dom `(:tr
            (:td ,completion)
            (:td :class "completion-annotation"
                 ,@(when (> (length annotation) 0)
                     (list annotation))))))))

(defmethod (setf completions)
    :after (new-val (buffer completion-list-mode))
  (with-current-buffer buffer
    (revert-buffer)))

;;; TAB completion

(define-mode active-completion-mode (completion-mode)
  ((replace-range)
   (completion-buffer
    :initform
    (make-buffer " *completion*"
                 :modes '(completion-list-mode))))
  (:documentation "Transient mode when completion menu is active."))

(define-keys prog-mode
  "tab" 'show-completions)

(define-keys active-completion-mode
  "up" 'previous-completion
  "down" 'next-completion
  "C-p" 'previous-completion
  "C-n" 'next-completion
  "C-v" 'scroll-down-completion
  "M-v" 'scroll-up-completion
  "C-g" 'hide-completions
  "enter" 'complete-selection)

(defgeneric compute-completion (buffer pos)
  (:documentation
   "Compute completions for BUFFER at POS.

Should return two values: REPLACE-RANGE, COMPLETIONS.

REPLACE-RANGE is a range which may be replaced by some completion.

COMPLETIONS is a list of completions. Each completion is a list of
form (text annotation)."))

(define-command show-completions (&optional (marker (focus)) silent)
  (bind (((:values replace-range completions)
          (compute-completion (current-buffer) marker)))
    (unless completions
      (unless silent (message "No completion."))
      (hide-completions)
      (return-from show-completions))
    (enable 'active-completion-mode)
    (setf (completions (completion-buffer (current-buffer)))
          completions
          (replace-range (current-buffer)) replace-range)
    t))

(defvar *completion-menu-size* (list 200 300)
  "Size of completion menu.

Should be a list of the form (WIDTH HEIGHT)")

(defmethod window-decoration-aux ((buffer active-completion-mode))
  (let* ((node (call-next-method))
         (main (only-elt (get-elements-by-class-name
                 node "main"))))
    (append-child
     main
     (dom `(:div :class "content completion-menu"
                 :style
                 ,(format nil "position: absolute; width: ~apx; height: ~apx; display: none;"
                          (car *completion-menu-size*)
                          (cadr *completion-menu-size*))
                 :buffer ,(id (completion-buffer buffer)))))
    node))

(defun compute-floating-buffer-position
    (rect width height max-width max-height)
  "Suggest position for displaying a floating buffer near RECT.

Assume buffer has WIDTH and HEIGHT.

Returns a list: (X Y)

X and Y are numbers in pixels."
  (bind (((x y w h) rect))
    (list
     (if (< (+ x w width) max-width)
         (+ x w)
         (- (+ x w) width))
     (if (< (+ y h height) max-height)
         (+ y h)
         (- y height)))))

(defun update-completion-menu-position (buffer)
  (when-let*
      ((menu (car
              (get-elements-by-class-name
               (window-decoration buffer)
               "completion-menu")))
       (buffer-bounds
        (evaluate-javascript-sync
         (ps:ps (ps:chain (js-buffer buffer) (get-bounds)))
         :global))
       (range (ignore-errors (replace-range buffer)))
       (completion-buffer
        (ignore-errors (completion-buffer buffer)))
       (min-width
        (evaluate-javascript-sync
         "document.body.scrollWidth"
         completion-buffer))
       (coord
        (compute-floating-buffer-position
         (get-bounding-client-rect (range-end range))
         min-width
         (cadr *completion-menu-size*)
         (assoc-value buffer-bounds :width)
         (assoc-value buffer-bounds :height))))
    (with-current-buffer (frame-root buffer)
      (bind (((x y) coord))
        (evaluate-javascript
         (ps:ps
           (let ((node (js-node-1 menu)))
             (setf (ps:chain node style left)
                   (ps:lisp (format nil "~apx" x))
                   (ps:chain node style top)
                   (ps:lisp (format nil "~apx" y))
                   (ps:chain node style min-width)
                   (ps:lisp (format nil "~apx" min-width))
                   (ps:chain node style display)
                   "block")))
         (current-buffer))))))

(defun maybe-hide-completions ()
  "Hide completions if focus move out of `replace-range'.

Return t if it hides completion, nil if it does nothing."
  (let ((buffer (current-buffer)))
    (unless (inside-range-inclusive-p
             (focus) (replace-range buffer))
      (hide-completions)
      t)))

(define-command hide-completions ()
  (disable 'active-completion-mode))

(defmethod on-post-command progn ((buffer active-completion-mode))
  (unless (maybe-hide-completions)
    (run-in-helper
     '*window-layout-helper*
     'update-completion-menu-position
     buffer)))

(defmethod disable-aux ((mode (eql 'active-completion-mode))
                        previous-instance)
  (delete-buffer (completion-buffer previous-instance)))

(define-command complete-selection ()
  (bind ((buffer (current-buffer))
         (row (node-after (focus (completion-buffer (current-buffer)))))
         (selection (text-content (first-child row)))
         (range (replace-range buffer))
         (end (range-end range)))
    (disable 'active-completion-mode)
    (with-marker (marker end)
      (delete-range range)
      (insert-nodes marker selection))))

;;; Auto completion

(define-mode auto-completion-mode ()
  ((minimum-prefix :default 3)
   (allowed-commands
    :default '(self-insert-command)
    :type (list-of symbol)))
  (:documentation
   "Automatically show completion menu after input.")
  (:toggler t))

(defmethod on-post-command progn ((buffer auto-completion-mode))
  (when (and (member *this-command* (allowed-commands buffer))
             (eq buffer (focused-buffer)))
    (when-let (node (node-containing (focus buffer)))
      (when (and (>= (length (text-content node))
                     (minimum-prefix buffer))
                 (show-completions (focus buffer) t))
        (run-in-helper
         '*window-layout-helper*
         'update-completion-menu-position
         buffer)))))

;;; Style

(defstyle completion-menu
    `(:white-space "nowrap"
      :font-size "0.8em"
      :overflow-x "hidden"
      :backdrop-filter "blur(10px)"
      :overflow-y "scroll"
      :border-collapse "collapse"))

(defstyle completion-match `(:color "#000"))
(defstyle completion-annotation `(:text-align "right"
                                  :font-style "italic"))
(defstyle completion-list-mode
    `(("body" :inherit completion-menu)
      (".completion-match" :inherit completion-match)
      (".completion-annotation" :inherit completion-annotation)))
