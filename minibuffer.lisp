(in-package #:neomacs)

(sera:export-always
    '(minibuffer-input-element minibuffer-input update-completion-buffer
      prompt read-from-minibuffer completing-read make-completion-buffer
      complete-minibuffer-aux))

(define-mode minibuffer-mode ()
  ((prompt :initarg :prompt)
   (initial :initform nil :initarg :initial)
   (completed :initform nil)
   (history-var :initform nil :initarg :history)
   (history-index :initform -1)
   (saved-input :initform nil)))

(define-keys minibuffer-mode
  "enter" 'exit-minibuffer
  "C-g" 'exit-recursive-edit
  "M-p" 'minibuffer-previous-history
  "M-n" 'minibuffer-next-history)

(defmethod selectable-p-aux ((buffer minibuffer-mode) pos)
  (pos-up-until pos (alex:rcurry #'class-p "input")))

(defmethod window-decoration-aux ((buffer minibuffer-mode))
  (dom `(:div :class "minibuffer" :selectable ""
              (:div :class "main content" :buffer ,(id buffer)))))

(define-command exit-minibuffer
  :mode minibuffer-mode ()
  (when-let (var (history-var (current-buffer)))
    (push (child-nodes (minibuffer-input-element (current-buffer)))
          (symbol-value var)))
  (setf (completed (current-buffer)) t))

(defun minibuffer-input-element (buffer)
  (only-elt (get-elements-by-class-name (document-root buffer) "input")))

(define-command minibuffer-previous-history
  :mode minibuffer-mode ()
  (let* ((buffer (current-buffer))
         (index (1+ (history-index buffer)))
         (history (symbol-value (or (history-var buffer)
                                    (error 'not-supported :operation 'history
                                                          :buffer buffer))))
         (input (minibuffer-input-element buffer)))
    (if (< index (length history))
        (progn
          (when (= (history-index buffer) -1)
            (setf (saved-input buffer)
                  (child-nodes input)))
          (setf (history-index buffer) index)
          (delete-nodes (pos-down input) nil)
          (apply #'insert-nodes (end-pos input)
                 (mapcar #'clone-node (nth index history))))
        (user-error "No previous history"))))

(define-command minibuffer-next-history
  :mode minibuffer-mode ()
  (let* ((buffer (current-buffer))
         (index (1- (history-index buffer)))
         (history (symbol-value (or (history-var buffer)
                                    (error 'not-supported :operation 'history
                                                          :buffer buffer))))
         (input (minibuffer-input-element buffer)))
    (if (>= index -1)
        (progn
          (setf (history-index buffer) index)
          (delete-nodes (pos-down input) nil)
          (apply #'insert-nodes (end-pos input)
                 (if (= index -1)
                     (saved-input buffer)
                     (mapcar #'clone-node (nth index history)))))
        (user-error "No next history"))))

(defgeneric minibuffer-input (buffer)
  (:method ((buffer minibuffer-mode))
    (text-content (minibuffer-input-element buffer)))
  (:documentation "Input text from BUFFER.

This is used for completion and should always return a string. To
return objects from `read-from-minibuffer', add methods to
`minibuffer-result'."))

(defgeneric minibuffer-result (buffer)
  (:method ((buffer minibuffer-mode))
    (minibuffer-input buffer))
  (:documentation "Result to be returned by `read-from-minibuffer'."))

(defmethod revert-buffer-aux ((buffer minibuffer-mode))
  (let ((input (dom `(:span :class "input"
                            ,@ (when (initial buffer)
                                 (list (initial buffer)))))))
    (setf (attribute (document-root (current-buffer))
                     'read-only)
          t)
    (insert-nodes (end-pos (document-root (current-buffer)))
                  (lret ((node (dom `(:span :class "prompt"
                                            ,(prompt buffer)))))
                    (setf (attribute node 'read-only) t))
                  input)
    (setf (pos (focus)) (end-pos input))))

(defun read-from-minibuffer (prompt &rest args)
  "Read and return a string from minibuffer with PROMPT.

ARGS are passed to `make-buffer' to create the minibuffer."
  (unless (getf args :mode)
    (setf (getf args :mode) 'minibuffer-mode))
  (let ((minibuf
          (apply #'make-buffer "*minibuffer*"
                 :prompt prompt args))
        (saved-focus (focused-buffer)))
    (split-window-below minibuf)
    (focus-buffer minibuf)
    (unwind-protect
         (with-current-buffer minibuf
           (revert-buffer)
           (recursive-edit (lambda () (not (completed minibuf))))
           (minibuffer-result minibuf))
      (focus-buffer saved-focus)
      (close-window minibuf)
      (delete-buffer minibuf))))

(defun read-yes-or-no (prompt)
  (let ((answer (read-from-minibuffer
                 (str:concat prompt "(yes or no) "))))
    (cond ((member answer '("y" "yes") :test 'equal)
           t)
          ((member answer '("n" "no") :test 'equal)
           nil)
          (t (read-yes-or-no prompt)))))

(defun completing-read (prompt list-mode &rest args)
  "Read and return a presentation from minibuffer with completion.

This is a wrapper around `read-from-minibuffer' that creates a completion buffer in LIST-MODE. The LIST-MODE should attach a presentation for each row, which will be returned by `completing-read'."
  (read-from-minibuffer
   prompt
   :mode 'minibuffer-completion-mode
   :completion-buffer
   (apply #'make-completion-buffer
          (list list-mode 'completion-buffer-mode)
          args)))

(define-mode completion-mode ()
  ((completion-buffer :initarg :completion-buffer))
  (:documentation
   "Abstract mode for buffers that has a completion buffer."))

(define-mode minibuffer-completion-mode
    (completion-mode minibuffer-mode) ()
  (:documentation
   "Mode for minibuffer that supports completion."))

(define-keys minibuffer-completion-mode
  "tab" 'complete-minibuffer
  'exit-minibuffer 'complete-exit-minibuffer
  'next-line 'next-completion
  'previous-line 'previous-completion
  'scroll-down-command 'scroll-down-completion
  'scroll-up-command 'scroll-up-completion
  'beginning-of-buffer 'first-completion
  'end-of-buffer 'last-completion)

(defgeneric update-completion-buffer (buffer)
  (:method ((buffer minibuffer-completion-mode))
    (setf (occur-query (completion-buffer buffer))
          (minibuffer-input buffer))))

(defgeneric complete-minibuffer-aux (buffer)
  (:method ((buffer minibuffer-completion-mode))
    (let ((input (only-elt (get-elements-by-class-name
                            (document-root buffer) "input")))
          (selection (node-after (focus (completion-buffer buffer)))))
      (when (selectable-p selection)
        (unless (class-p selection "dummy-row")
          (delete-nodes (pos-down input) nil)
          (insert-nodes (pos-down input)
                        (text-content (first-child selection))))))))

(defmethod on-post-command progn ((buffer minibuffer-completion-mode))
  (update-completion-buffer buffer))

(defmethod minibuffer-result ((buffer minibuffer-completion-mode))
  (if (require-match (completion-buffer buffer))
      (let ((node (node-after (focus (completion-buffer buffer)))))
        (when (selectable-p node)
          (attribute node 'presentation)))
      (minibuffer-input buffer)))

(define-mode completion-buffer-mode (list-mode)
  ((rows :initform nil)
   (occur-query :initform "")
   (completion-limit :default 300)
   (require-match
    :initform t :initarg :require-match
    :documentation
    "Whether a match is required.

If this slot is NIL, an invisible selectable dummy row is inserted at
the beginning of the completion buffer. No completion is performed
when this row is selected.")))

(defmethod generate-rows :around ((buffer completion-buffer-mode))
  (if (require-match buffer) (call-next-method)
      (cons (make-element "tr" :class "dummy-row")
            (call-next-method))))

(defmethod revert-buffer-aux ((buffer completion-buffer-mode))
  (erase-buffer)
  (let ((body-node (make-element "tbody")))
    (insert-nodes (end-pos (document-root buffer))
                  (make-element "table" :children (list body-node)))
    (setf (restriction buffer) body-node)
    (setf (rows buffer) (generate-rows buffer))
    (iter (for row in (rows buffer))
      (for _ below (completion-limit buffer))
      (insert-nodes (end-pos body-node) row))
    (setf (pos (focus))
          (or (npos-right-ensure
               (pos-down body-node)
               (lambda (p) (not (class-p p "dummy-row"))))
              (error 'top-of-subtree)))))

(defmethod occur-p-aux :around ((buffer completion-buffer-mode)
                                (query t) element)
  (if (class-p element "dummy-row") t (call-next-method)))

(defmethod (setf occur-query) :around (new-val (buffer completion-buffer-mode))
  (let ((old-val (slot-value buffer 'occur-query)))
    (prog1 (call-next-method)
      (unless (equal old-val new-val)
        (with-current-buffer buffer
          (let* ((*inhibit-read-only* t)
                 (body-node (last-child (first-child (document-root buffer))))
                 (pos (pos-down body-node)))
            (clear-match-highlight buffer)
            (iter (with i = 0)
              (for row in (rows buffer))
              (for matches = (occur-p-aux buffer new-val row))
              (when matches
                (if (host row)
                    (progn
                      (delete-nodes pos row)
                      (setf pos (pos-right row)))
                    (insert-nodes pos row))
                (render-match-highlight buffer matches)
                (incf i))
              (while (< i (completion-limit buffer))))
            (delete-nodes pos nil)))))))

(defsheet completion-buffer-mode
    `(("dummy-row" :display "none")
      ("::highlight(occur)" :inherit match)))

(defmethod selectable-p-aux ((buffer completion-buffer-mode) pos)
  (tag-name-p (node-after pos) "tr"))

(defmethod on-mouse-click progn ((buffer completion-buffer-mode)
                                 (x t) (y t))
  (setf (adjust-marker-direction buffer) 'backward))

(defmethod window-decoration-aux ((buffer minibuffer-completion-mode))
  (dom `(:div :class "buffer" :selectable ""
              (:div :class "main content" :style "flex:0 0 2em;"
                    :buffer ,(id buffer))
              (:div :class "content"
                    :buffer ,(id (completion-buffer buffer))))))

(defmethod on-delete-buffer progn ((buffer minibuffer-completion-mode))
  (delete-buffer (completion-buffer buffer)))

(define-command complete-minibuffer
  :mode minibuffer-completion-mode ()
  (complete-minibuffer-aux (current-buffer)))

(define-command complete-exit-minibuffer
  :mode minibuffer-completion-mode ()
  (complete-minibuffer)
  (exit-minibuffer))

(define-command next-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (forward-element)))

(define-command previous-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (backward-element)))

(define-command scroll-down-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (dotimes (_ (scroll-lines (current-buffer)))
      (forward-element))))

(define-command scroll-up-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (dotimes (_ (scroll-lines (current-buffer)))
      (backward-element))))

(define-command first-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (beginning-of-buffer)))

(define-command last-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (end-of-buffer)))

(defun make-completion-buffer (modes &rest args)
  (apply #'make-buffer " *completion*" :mode modes :revert t
         args))

(defun collect-keybindings (keymaps)
  (lret (keys (table (make-hash-table)))
    (iter (for keymap in keymaps)
      (traverse-keymap
       keymap (lambda (kseq cmd)
                (declare (ignore cmd))
                (push kseq keys))))
    (iter (for k in (delete-duplicates keys :test 'equal))
      (push k (gethash (lookup-keybind k keymaps) table)))))

(define-command execute-command ()
  (call-interactively
   (completing-read
    "M-x " 'command-list-mode
    :include-modes (modes (current-buffer))
    :keybinding-index
    (collect-keybindings (keymaps (current-buffer))))))

(define-keys :global
  "M-x" 'execute-command)

(defstyle minibuffer-prompt `(:inherit bold))

(defsheet minibuffer `((".prompt" :inherit minibuffer-prompt)))
