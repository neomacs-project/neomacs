(in-package #:neomacs)

(define-condition top-level () ()
  (:report "Return to top-level command loop"))

(define-condition exit-recursive-edit ()
  ((condition :initform nil :initarg :condition))
  (:report "Return from recursive edit"))

(define-condition quit () ()
  (:report "Quit"))

(defvar *event-queue* (sb-concurrency:make-mailbox))
(defvar *last-command* nil)
(defvar *this-command* nil)
(defvar *this-command-keys* nil)
(defvar *debug-on-error* nil)

(define-command toggle-debug-on-error ()
  (setq *debug-on-error* (not *debug-on-error*))
  (message "Debug on error ~:[disabled~;enabled~]" *debug-on-error*))

(defun run-command (command)
  (setf *last-command* *this-command*
        *this-command* command)
  (message nil)
  (unwind-protect
       (call-with-current-buffer (focused-buffer) command)
    (setq *this-command-keys* nil)))

(defun handle-event (buffer event)
  (let ((type (assoc-value event :type)))
    (cond ((equal type "keyDown")
           (unless (eql (focused-buffer) buffer)
             (warn "Neomacs and Electron has different idea of focused buffer:~% ~a vs ~a"
                   (focused-buffer) buffer)
             (setq buffer (focused-buffer))
             (when (focused-buffer)
               (evaluate-javascript
                (ps:ps (ps:chain (js-buffer buffer) web-contents (focus)))
                nil)))
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
                       (message "~a-" (keys-description *this-command-keys*)))
                     (run-command cmd))
                 (progn
                   (message "~a is undefined" (keys-description *this-command-keys*))
                   (setq *this-command-keys* nil))))))
          ((equal type "load")
           (with-current-buffer buffer
             (on-buffer-loaded buffer)))
          ((equal type "title-updated")
           (with-current-buffer buffer
             (on-buffer-title-updated
              buffer (assoc-value event :title))))
          ((equal type "keyUp"))
          (t (warn "Unrecoginized Electron event: ~a" event)))))

(defun command-loop (&optional recursive-p)
  (let (exit-condition *this-command-keys*)
    (iter (for data = (sb-concurrency:receive-message *event-queue*))
      (until (eql data 'quit))
      (for buffer = (gethash (parse-integer (assoc-value data :buffer)) *buffer-table*))
      (for event = (assoc-value data :input-event))
      (restart-case
          (handler-bind
              ((quit (lambda (c)
                       (declare (ignore c))
                       (evaluate-javascript
                        "new Audio('https://www.myinstants.com/media/sounds/vine-boom.mp3').play()"
                        (current-frame-root))
                       (message "Quit")
                       (next-iteration)))
               (error (lambda (c)
                        (unless *debug-on-error*
                          (evaluate-javascript
                           "new Audio('https://www.myinstants.com/media/sounds/amogus.mp3').play()"
                           (current-frame-root))
                          (message "~a" c)
                          (next-iteration)))))
            (handler-case
                (handle-event buffer event)
              (top-level ()
                (when recursive-p (error 'top-level)))
              (exit-recursive-edit (c)
                (when recursive-p
                  (setq exit-condition (slot-value c 'condition))
                  (return)))))
        (abort ()
          :report "Return to command loop")))
    (when exit-condition (error exit-condition))))

(defun recursive-edit ()
  (cleanup-locked-buffers)
  (lwcells::evaluate-activations)
  (let (*locked-buffers* lwcells::*delay-evaluation-p*)
    (command-loop t)))

(defvar *command-loop-thread* nil)

(defun start-command-loop ()
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
  (error 'quit))

(define-keys global
  "C-g" 'keyboard-quit)
