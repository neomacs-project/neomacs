(in-package #:neomacs)

(sera:export-always
    '(top-level exit-recursive-edit quit
      call-with-current-buffer with-current-buffer
      *last-command* *this-command* *this-command-keys*
      recursive-edit start-command-loop
      *debug-on-error* *message-log-max*
      *quit-hook* *error-hook*
      play-loud-audio do-nothing))

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

(defvar *locked-buffers* nil)

(defvar *post-command-buffers* nil)

(defun cleanup-locked-buffers ()
  (let ((*post-command-buffers* *post-command-buffers*))
    (iter (for saved = *post-command-buffers*)
      (setq *post-command-buffers* nil)
      (dolist (buffer saved)
        (when (buffer-alive-p buffer)
          (let ((*current-buffer* buffer))
            (case (adjust-marker-direction buffer)
              ((forward) (ensure-selectable (focus buffer)))
              ((backward) (ensure-selectable (focus buffer) t)))
            (on-post-command buffer)
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
           (setf (adjust-marker-direction buffer) 'forward)
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
         (setf (adjust-marker-direction buffer) 'forward)
         (let ((*current-buffer* buffer))
           (on-pre-command buffer)
           (funcall thunk)))))

(defvar *event-queue* (sb-concurrency:make-mailbox))

(defvar *last-command* nil
  "Last command run by command loop.")

(defvar *this-command* nil
  "Current command run by command loop.

The value is bound to nil outside command invocation made by command
loop.")

(defvar *this-command-keys* nil
  "List of keys that cause current command to run.

the value is bound to nil outside command invocation made by command
loop.")

(defvar *debug-on-error* nil)

(defvar *use-neomacs-debugger* nil)

(defvar *message-log-max* 1000
  "Maximum number of lines to keep in the `*Messages*' buffer.

If nil, disable message logging. If t, log messages but don't truncate
`*Messages*' buffer.")

(defun play-loud-audio (c)
  (if (or (typep c 'quit) (typep c 'user-error))
      (evaluate-javascript
       "new Audio('https://www.myinstants.com/media/sounds/vine-boom.mp3').play()"
       (current-frame-root))
      (evaluate-javascript
       "new Audio('https://www.myinstants.com/media/sounds/amogus.mp3').play()"
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
  (setq *debug-on-error* (not *debug-on-error*))
  (message "Debug on error ~:[disabled~;enabled~]" *debug-on-error*))

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

(defun handle-event (buffer event run-command-fn)
  (let ((type (assoc-value event :type)))
    (cond ((equal type "keyDown")
           (unless (eql (focused-buffer) buffer)
             (warn "Neomacs and Electron has different idea of focused buffer:~% ~a vs ~a"
                   (focused-buffer) buffer)
             (setq buffer (focused-buffer))
             (when (focused-buffer)
               (evaluate-javascript
                (ps:ps (ps:chain (js-buffer buffer) web-contents (focus)))
                :global)))
           (let ((key (make-key :ctrl (assoc-value event :control)
                                :meta (assoc-value event :alt)
                                :super (assoc-value event :meta)
                                :shift (assoc-value event :shift)
                                :sym (assoc-value event :code))))
             (unless (member (key-sym key)
                             '("ControlLeft" "ControlRight"
                               "MetaLeft" "MetaRight"
                               "AltLeft" "AltRight"
                               "ShiftLeft" "ShiftRight")
                             :test 'equal)
               (alex:nconcf *this-command-keys* (list key))
               (if-let (cmd (lookup-keybind *this-command-keys* (keymaps buffer)))
                 (if (prefix-command-p cmd)
                     (let ((*message-log-max* nil))
                       (message "~a-" (key-description *this-command-keys*)))
                     (funcall run-command-fn cmd))
                 (funcall run-command-fn nil)))))
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
          ((equal type "keyUp"))
          ((equal type "did-start-navigation")
           (with-current-buffer buffer
             (on-buffer-did-start-navigation
              buffer (assoc-value event :url))))
          ((equal type "new-buffer")
           (switch-to-buffer
            (make-buffer "Web" :mode 'web-mode
                               :id (assoc-value event :new-id)
                               :url (assoc-value event :url)
                               :styles nil)))
          ((eq type 'debug-request)
           (debug-for-environment
            (assoc-value event :environment)
            (assoc-value event :mailbox)))
          (t (warn "Unrecoginized Electron event: ~a" event)))))

(defun command-loop (&optional recursive-p
                       (guard-fn (constantly t))
                       (handlers-p t)
                       (run-command-fn #'run-command))
  (when *use-neomacs-debugger*
    (trivial-custom-debugger:install-debugger
     #'neomacs-debugger-hook))
  (let (*this-command-keys*)
    (iter (for data = (sb-concurrency:receive-message *event-queue*))
      (until (eql data 'quit))
      (for buffer = (gethash (parse-integer (assoc-value data :buffer)) *buffer-table*))
      (for event = (assoc-value data :input-event))
      (if handlers-p
          (restart-case
              (handler-bind
                  ((quit (lambda (c)
                           (funcall *quit-hook* c)
                           (message "Quit")
                           (next-iteration)))
                   (user-error
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
                       (when recursive-p (error 'quit))))
                   (top-level
                     (lambda (c)
                       (declare (ignore c))
                       (unless recursive-p (next-iteration)))))
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
         (command-loop t guard-fn handlers-p run-command-fn))
    (dolist (buffer *locked-buffers*)
      (bt:acquire-lock (lock buffer))))
  nil)

(defvar *command-loop-thread* nil)

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
        (bt:make-thread #'command-loop :name "Neomacs Command Loop")))

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

(defun helper-thread-loop ()
  (iter (with mailbox = (gethash (bt:current-thread) *helper-mailboxes*))
    (for message = (sb-concurrency:receive-message mailbox))
    (let ((all-messages
            (delete-duplicates
             (cons message
                   (sb-concurrency:receive-pending-messages
                    mailbox))
             :test 'equal :from-end t)))
      (iter (for message in all-messages)
        (with-demoted-errors
            (format nil "Error in helper ~a" (bt:current-thread))
          (apply (car message) (cdr message)))))))

;; TODO: Make `ensure-helper-thread' itself thread-safe? Is this
;; needed? If it is only ever called from command loop then it's not
;; needed
(defun ensure-helper-thread (symbol)
  (let ((thread (symbol-value symbol)))
    (unless (and thread (bt:thread-alive-p thread))
      (setq thread
            (bt:make-thread
             'helper-thread-loop
             :name (string-downcase (symbol-name symbol))))
      (setf (symbol-value symbol) thread
            (gethash thread *helper-mailboxes*)
            (sb-concurrency:make-mailbox)))
    symbol))

(defun run-in-helper (symbol function &rest args)
  (ensure-helper-thread symbol)
  (sb-concurrency:send-message
   (gethash (symbol-value symbol) *helper-mailboxes*)
   (cons function args)))
