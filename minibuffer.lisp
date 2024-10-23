(in-package #:neomacs)

(define-class minibuffer-mode () ((prompt :initarg :prompt)))

(define-keymap minibuffer-mode ()
  "enter" 'exit-minibuffer
  "C-g" 'abort-minibuffer)

(defmethod selectable-p-aux ((buffer minibuffer-mode) pos)
  (class-p (node-containing pos) "input"))

(defmethod enable-aux ((mode (eql 'minibuffer-mode)))
  (setf (window-decoration (current-buffer))
        (dom `((:div :class "minibuffer" :selectable "")
               ((:div :class "content" :buffer ,(id (current-buffer))))))))

(define-command exit-minibuffer ()
  (error 'exit-recursive-edit))

(define-command abort-minibuffer ()
  (error 'exit-recursive-edit :condition 'quit))

(defun minibuffer-input-element (buffer)
  (only-elt (get-elements-by-class-name (document-root buffer) "input")))

(defmethod minibuffer-input ((buffer minibuffer-mode))
  (text-content (minibuffer-input-element buffer)))

(defmethod revert-buffer-aux ((buffer minibuffer-mode))
  (let ((input (dom `((:span :class "input")))))
    (insert-nodes (end-pos (document-root (current-buffer)))
                  (dom `((:span :class "prompt")
                         ,(prompt buffer)))
                  input)
    (setf (pos (focus)) (end-pos input))))

(defun read-from-minibuffer (prompt &rest args)
  "Read and return a string from minibuffer with PROMPT.

ARGS are passed to `make-buffer' to create the minibuffer."
  (unless (getf args :modes)
    (setf (getf args :modes) 'minibuffer-mode))
  (let ((minibuf
          (apply #'make-buffer "*minibuffer*"
                 :prompt prompt args))
        (saved-focus (focused-buffer)))
    (display-buffer-below minibuf)
    (focus-buffer minibuf)
    (unwind-protect
         (with-current-buffer minibuf
           (revert-buffer)
           (recursive-edit)
           (minibuffer-input minibuf))
      (focus-buffer saved-focus)
      (close-buffer-display minibuf)
      (delete-buffer minibuf))))

(defun completing-read (prompt list-mode)
  "Read and return a string from minibuffer with completion.

This is a thin wrapper around `read-from-minibuffer' that creates a completion buffer in LIST-MODE."
  (read-from-minibuffer
   prompt
   :modes 'minibuffer-completion-mode
   :completion-buffer
   (make-completion-buffer
    (list list-mode 'completion-buffer-mode))))

(define-class minibuffer-completion-mode (minibuffer-mode)
  ((completion-buffer :initarg :completion-buffer))
  (:documentation
   "Mode for minibuffer that supports completion."))

(define-keymap minibuffer-completion-mode ()
  "tab" 'complete-minibuffer
  'exit-minibuffer 'complete-exit-minibuffer
  'next-line 'next-minibuffer-completion
  'previous-line 'previous-minibuffer-completion
  'scroll-down-command 'scroll-down-minibuffer-completion
  'scroll-up-command 'scroll-up-minibuffer-completion
  'beginning-of-buffer 'beginning-of-minibuffer-completion
  'end-of-buffer 'end-of-minibuffer-completion)

(defgeneric update-completion-buffer (buffer)
  (:method ((buffer minibuffer-completion-mode))
    (setf (occur-query (completion-buffer buffer))
        (minibuffer-input buffer))))

(defmethod on-post-command progn ((buffer minibuffer-completion-mode))
  (update-completion-buffer buffer))

(define-class completion-buffer-mode (occur-mode) ())

(defmethod selectable-p-aux ((buffer completion-buffer-mode) pos)
  (tag-name-p (node-after pos) "tr"))

(defmethod initialize-instance :after
    ((buffer minibuffer-completion-mode) &key)
  (setf (window-decoration buffer)
        (dom `((:div :class "buffer" :selectable "")
               ((:div :class "content" :style "flex:0 0 2em;"
                      :buffer ,(id buffer)))
               ((:div :class "content"
                      :buffer ,(id (completion-buffer buffer))))))))

(defmethod on-delete-buffer progn ((buffer minibuffer-completion-mode))
  (delete-buffer (completion-buffer buffer)))

(define-command complete-minibuffer ()
  (let ((input (only-elt (get-elements-by-class-name
                          (document-root (current-buffer)) "input"))))
    (delete-nodes (pos-down input) nil)
    (insert-nodes (pos-down input)
                  (text-content (first-child (node-after (focus (completion-buffer (current-buffer)))))))))

(define-command complete-exit-minibuffer ()
  (complete-minibuffer)
  (exit-minibuffer))

(define-command next-minibuffer-completion ()
  (with-current-buffer (completion-buffer (current-buffer))
    (forward-element)))

(define-command previous-minibuffer-completion ()
  (with-current-buffer (completion-buffer (current-buffer))
    (backward-element)))

(define-command scroll-down-minibuffer-completion ()
  (with-current-buffer (completion-buffer (current-buffer))
    (dotimes (_ (scroll-lines (current-buffer)))
      (forward-element))))

(define-command scroll-up-minibuffer-completion ()
  (with-current-buffer (completion-buffer (current-buffer))
    (dotimes (_ (scroll-lines (current-buffer)))
      (backward-element))))

(define-command beginning-of-minibuffer-completion ()
  (with-current-buffer (completion-buffer (current-buffer))
    (beginning-of-buffer)))

(define-command end-of-minibuffer-completion ()
  (with-current-buffer (completion-buffer (current-buffer))
    (end-of-buffer)))

(defun make-completion-buffer (modes &rest args)
  (lret ((buffer (apply #'make-buffer "*completion*" :modes modes
                        args)))
    (with-current-buffer buffer
      (revert-buffer))))

(define-command execute-command ()
  (let ((name (completing-read "M-x " 'command-list-mode)))
    (if-let (cmd (find name *commands*
                       :key (alex:compose #'string-downcase #'symbol-name)
                       :test 'equal))
      (funcall cmd)
      (message "No such command"))))

(define-key *global-keymap*
  "M-x" 'execute-command)

(defstyle minibuffer-prompt `(:inherit bold))

(defstyle minibuffer `((".prompt" :inherit minibuffer-prompt)))
