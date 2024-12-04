(in-package #:neomacs)

(sera:export-always
    '(top-level exit-recursive-edit quit
      call-with-current-buffer with-current-buffer
      *last-command* *this-command* *this-command-keys*
      *input-method-function*
      current-frame-root recursive-edit start-command-loop
      *debug-on-error* *message-log-max*
      *quit-hook* *error-hook*
      play-loud-audio do-nothing
      read-key read-key-sequence))

(define-condition top-level () ()
  (:report "Return to top-level command loop"))

(define-condition exit-recursive-edit () ()
  (:report "Return from recursive edit")
  (:documentation
   "Cause recursive edit level to exit.

This is handled by the innermost recursive edit level started with
non-nil HANDLER-P, which would return and signal a `quit' condition."))

(define-condition quit () ()
  (:report "Quit"))

(define-condition async-quit (quit) ()
  (:report "Quit (async interrupt)"))

(defvar *locked-buffers* nil)

(defvar *post-command-buffers* nil)

(defun cleanup-locked-buffers ()
  (let ((*post-command-buffers* *post-command-buffers*))
    (iter (for saved = *post-command-buffers*)
      (setq *post-command-buffers* nil)
      (dolist (buffer saved)
        (when (buffer-alive-p buffer)
          (let ((*current-buffer* buffer))
            (on-post-command buffer)
            (ensure-selectable (focus buffer))
            (let ((focus (pos (focus buffer)))
                  (selection (pos (selection-marker buffer))))
              (render-focus focus)
              (clear-range-selection buffer)
              (when (selection-active buffer)
                (render-range-selection
                 (range selection focus)))))))
      (while *post-command-buffers*))))

(defun call-with-current-buffer (buffer thunk)
  (check-type buffer buffer)
  (cond ((not *locked-buffers*)
         (let ((*locked-buffers* (list buffer))
               (*post-command-buffers* (list buffer))
               (*current-buffer* buffer))
           (bt:acquire-lock (lock buffer))
           (unwind-protect
                (progn
                  (on-pre-command buffer)
                  (funcall thunk))
             (unwind-protect (cleanup-locked-buffers)
               (dolist (buffer *locked-buffers*)
                 (bt:release-lock (lock buffer)))))))
        ((member buffer *locked-buffers*)
         (pushnew buffer *post-command-buffers*)
         (let ((*current-buffer* buffer))
           (funcall thunk)))
        (t
         (push buffer *locked-buffers*)
         (pushnew buffer *post-command-buffers*)
         (bt:acquire-lock (lock buffer))
         (let ((*current-buffer* buffer))
           (on-pre-command buffer)
           (funcall thunk)))))

(defvar *last-command* nil
  "Last command run by command loop.")

(defvar *this-command* nil
  "Current command run by command loop.

The value is bound to nil outside command invocation made by command
loop.")

(defvar *this-command-keys* nil
  "List of keys that cause current command to run.

The value is bound to nil outside command invocation made by command
loop.")

(defvar *input-method-function* #'list
  "Function that implements the current input method.

It's called with each key received by the command loop, and should
return a list of keys which are then used for command dispatch.")

(defvar *debug-on-error* nil)

(defvar *no-debugger* nil)

(defvar *no-redirect-stream* nil)

(defvar *no-init* nil)

(defvar *message-log-max* 1000
  "Maximum number of lines to keep in the `*Messages*' buffer.

If nil, disable message logging. If t, log messages but don't truncate
`*Messages*' buffer.")

(defun play-loud-audio (c)
  (if (or (typep c 'quit) (typep c 'user-error))
      (evaluate-javascript
       "new Audio('neomacs://sys/vine-boom.mp3').play()"
       (current-frame-root))
      (evaluate-javascript
       "new Audio('neomacs://sys/amogus.mp3').play()"
       (current-frame-root))))

(defun do-nothing (&rest args)
  (declare (ignore args)))

(defvar *quit-hook* 'play-loud-audio
  "Invoked when a quit condition reaches command loop.

`user-error's are considered quits and also trigger this hook.")

(defvar *error-hook* 'play-loud-audio
  "Invoked when an error condition reaches command loop.

`user-error's are considered quits and does not trigger this hook.")

(define-command toggle-debug-on-error ()
  (setf (sb-ext:symbol-global-value '*debug-on-error*)
        (not (sb-ext:symbol-global-value '*debug-on-error*)))
  (message "Debug on error ~:[disabled~;enabled~]" *debug-on-error*))

(defvar *current-frame-root* nil)

(defun current-frame-root ()
  *current-frame-root*)

(defun run-command (command)
  (if command
      (let ((*this-command* command))
        (message nil)
        (unwind-protect
             (with-current-buffer (focused-buffer)
               (call-interactively command))
          (setq *this-command-keys* nil
                *last-command* *this-command*)))
      (progn
        (message "~a is undefined" (key-description *this-command-keys*))
        (setq *this-command-keys* nil))))

(defun handle-key (buffer key run-command-fn)
  (alex:nconcf *this-command-keys* (list key))
  (if-let (cmd (lookup-keybind *this-command-keys* (keymaps buffer)))
    (if (prefix-command-p cmd)
        (let ((*message-log-max* nil))
          (message "~a-" (key-description *this-command-keys*)))
        (funcall run-command-fn cmd))
    (funcall run-command-fn nil)))

(defun handle-event (buffer event run-command-fn)
  (let ((type (assoc-value event :type)))
    (cond ((equal type "keyDown")
           (if-let (frame-root (and buffer (frame-root buffer)))
             (setf *current-frame-root* frame-root)
             (warn "~a got Electron focus but does not belong to a frame root" buffer))
           (unless (eql (focused-buffer) buffer)
             #+nil (warn "Neomacs and Electron has different idea of focused buffer:~% ~a vs ~a"
                   (focused-buffer) buffer)
             (setq buffer (focused-buffer))
             (when (focused-buffer)
               (evaluate-javascript
                (ps:ps (ps:chain (js-buffer buffer) web-contents (focus)))
                :global)))
           (let* ((sym (assoc-value event :key))
                  (key (make-key :ctrl (assoc-value event :control)
                                 :meta (assoc-value event :alt)
                                 :super (assoc-value event :meta)
                                 ;; If it is a single char, discard
                                 ;; shift modifier because the
                                 ;; translation has probably taken it
                                 ;; into account already.
                                 :shift (and (> (length sym) 1)
                                             (assoc-value event :shift))
                                 :sym (cond ((equal sym " ") "Space")
                                            ((equal sym "Dead")
                                             (str:concat sym (assoc-value event :code)))
                                            (t sym)))))
             (unless (member (key-sym key)
                             '("Control" "Meta" "Alt" "Shift")
                             :test 'equal)
               (dolist (key (funcall *input-method-function* key))
                 (handle-key buffer key run-command-fn)))))
          ((equal type "load")
           (with-current-buffer buffer
             (on-buffer-loaded
              buffer (assoc-value event :url) nil)))
          ((equal type "fail-load")
           (with-current-buffer buffer
             (on-buffer-loaded
              buffer (assoc-value event :url)
              (or (assoc-value event :err)
                  (list (cons :code "UNKNOWN"))))))
          ((equal type "dom-ready")
           (with-current-buffer buffer
             (on-buffer-dom-ready buffer)))
          ((equal type "title-updated")
           (with-current-buffer buffer
             (on-buffer-title-updated
              buffer (assoc-value event :title))))
          ((equal type "did-start-loading")
           (when buffer (setf (load-spinner buffer) t)))
          ((equal type "did-stop-loading")
           (when buffer (setf (load-spinner buffer) nil)))
          ((equal type "keyUp"))
          ((equal type "click")
           (when buffer
             (with-current-buffer buffer
               (on-mouse-click buffer (assoc-value event :x)
                               (assoc-value event :y)))))
          ((equal type "did-start-navigation")
           (when buffer
             (with-current-buffer buffer
               (on-buffer-did-start-navigation buffer event))))
          ((equal type "new-buffer")
           (switch-to-buffer
            (make-buffer "Web" :mode 'web-mode
                               :id (assoc-value event :new-id)
                               :url (assoc-value event :url)
                               :styles nil)))
          ((equal type "frame-closed")
           (delete-buffer buffer))
          ((equal type "frame-focused")
           (with-current-buffer buffer nil))
          ((equal type "focus") (focus-buffer buffer))
          ((equal type "enter-html-full-screen")
           (with-current-buffer (frame-root buffer)
             (delete-other-windows buffer)
             (enable 'fullscreen-mode)))
          ((equal type "leave-html-full-screen")
           (with-current-buffer (frame-root buffer)
             (disable 'fullscreen-mode)))
          ((eq type 'debug-request)
           (debug-for-environment
            (assoc-value event :environment)
            (assoc-value event :mailbox)))
          (t (warn "Unrecoginized Electron event: ~a" event)))))

(defun command-loop (&optional
                       (guard-fn (constantly t))
                       (handlers-p t)
                       (run-command-fn #'run-command))
  (let (*this-command-keys*)
    (iter (setq *last-quit-time* nil)
      (for data = (sb-concurrency:receive-message *event-queue*))
      (until (eql data 'quit))
      (for buffer = (gethash (parse-integer (assoc-value data :buffer)) *buffer-table*))
      (for event = (assoc-value data :input-event))
      (if handlers-p
          (restart-case
              (handler-bind
                  (((or quit user-error)
                     (lambda (c)
                       (funcall *quit-hook* c)
                       (message "~a" c)
                       (next-iteration)))
                   (error (lambda (c)
                            (unless *debug-on-error*
                              (funcall *error-hook* c)
                              (message "~a" c)
                              (next-iteration))))
                   (exit-recursive-edit
                     (lambda (c)
                       (declare (ignore c))
                       (error 'quit))))
                (handle-event buffer event run-command-fn))
            (abort ()
              :report "Return to command loop"))
          (handle-event buffer event run-command-fn))
      (while (funcall guard-fn)))))

(defun recursive-edit (&optional (guard-fn (constantly t))
                         (handlers-p t)
                         (run-command-fn #'run-command))
  "Start a recursive edit level.

GUARD-FN is called after every command invocation and the level exits
if GUARD-FN returns nil. If HANDLERS-P is non-nil, set up condition
and restart handlers."
  (cleanup-locked-buffers)
  (dolist (buffer *locked-buffers*)
    (bt:release-lock (lock buffer)))
  (unwind-protect
       (let (*locked-buffers*)
         (command-loop guard-fn handlers-p run-command-fn))
    (dolist (buffer *locked-buffers*)
      (bt:acquire-lock (lock buffer))))
  nil)

(defun read-key-sequence (prompt)
  (let (*message-log-max*)
    (message "~a" prompt))
  (recursive-edit
   (constantly t) nil
   (lambda (cmd)
     (declare (ignore cmd))
     (message nil)
     (return-from read-key-sequence *this-command-keys*))))

(defun read-key (prompt)
  (let (*message-log-max*)
    (message "~a" prompt))
  (recursive-edit
   (lambda ()
     (when *this-command-keys*
       (return-from read-key (lastcar *this-command-keys*))))
   nil
   (lambda (cmd)
     (declare (ignore cmd))
     (message nil)
     (return-from read-key (lastcar *this-command-keys*)))))

(defun top-level ()
  (unless *no-debugger*
    (trivial-custom-debugger:install-debugger
     #'neomacs-debugger-hook))
  (unless *no-redirect-stream*
    (setup-stream-indirection))
  (sb-thread:with-new-session ()
    (iter (handler-case (command-loop)
            (quit () (message "Already at top level"))
            (top-level ())))))

(defun start-command-loop ()
  "Start Neomacs command loop.

If a command loop is already running, ask and wait for it to quit
before starting a new one. This is useful when you want changes to the
function `command-loop' to take effect."
  (when (and *command-loop-thread*
             (bt:thread-alive-p *command-loop-thread*))
    (format t "Waiting for command loop to quit...")
    (sb-concurrency:send-message *event-queue* 'quit)
    (bt:join-thread *command-loop-thread*)
    (format t "ok~%"))
  (setq *command-loop-thread*
        (bt:make-thread #'top-level :name "Neomacs Command Loop")))

(define-command keyboard-quit ()
  "Signal a `quit' condition."
  (setf (selection-active (current-buffer)) nil)
  (error 'quit))

(define-command exit-recursive-edit ()
  "Exit current recursive edit level."
  (error 'exit-recursive-edit))

(define-keys :global
  "C-g" 'keyboard-quit)

;;; Helper threads

(defvar *helper-mailboxes*
  (make-hash-table :weakness :key))

(defvar *helper-lock*
  (bt2:make-lock :name "helper register lock"))

(defun helper-thread-loop ()
  (iter (with mailbox =
              (bt2:with-lock-held (*helper-lock*)
                (gethash (bt:current-thread) *helper-mailboxes*)))
    (for message = (sb-concurrency:receive-message mailbox))
    (let ((all-messages
            (delete-duplicates
             (cons message
                   (sb-concurrency:receive-pending-messages
                    mailbox))
             :test 'equal :from-end t)))
      (iter (for message in all-messages)
        (with-demoted-errors
            (format nil "Error in helper ~a:~%" (bt:current-thread))
          (apply (car message) (cdr message)))))))

(defun ensure-helper-thread (symbol)
  (bt2:with-lock-held (*helper-lock*)
    (let ((thread (symbol-value symbol)))
      (unless (and thread (bt:thread-alive-p thread))
        (setq thread
              (bt:make-thread
               'helper-thread-loop
               :name (string-downcase (symbol-name symbol))))
        (setf (symbol-value symbol) thread
              (gethash thread *helper-mailboxes*)
              (sb-concurrency:make-mailbox)))
      symbol)))

(defun run-in-helper (symbol function &rest args)
  (ensure-helper-thread symbol)
  (sb-concurrency:send-message
   (gethash (symbol-value symbol) *helper-mailboxes*)
   (cons function args)))
