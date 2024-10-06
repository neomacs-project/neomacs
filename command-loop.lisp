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
(defvar *debug-on-error* t)

(defun run-command (buffer command)
  (restart-case
      (let ()
        (setf *last-command* *this-command*
              *this-command* command)
        (echo nil)
        (call-with-buffer-transaction buffer command))
    (abort ()
      :report "Return to command loop")))

(defun command-loop (&optional recursive-p)
  (let (exit-condition)
    (iter (for data = (sb-concurrency:receive-message *event-queue*))
      (until (eql data 'quit))
      (for buffer = (gethash (parse-integer (assoc-value data :buffer)) *buffer-table*))
      (for event = (assoc-value data :input-event))
      (with prefix-keys = nil)
      (handler-bind
          ((quit (lambda (c)
                   (declare (ignore c))
                   (evaluate-javascript
                    "new Audio('https://www.myinstants.com/media/sounds/vine-boom.mp3').play()"
                    (current-frame-root))
                   (echo "Quit")
                   (next-iteration)))
           (error (lambda (c)
                    (unless *debug-on-error*
                      (echo "~a" c)
                      (next-iteration)))))
        (handler-case
            (let ((type (assoc-value event :type)))
              (cond ((equal type "keyDown")
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
                         (alex:nconcf prefix-keys (list key))
                         (if-let (cmd (lookup-keybind prefix-keys (keymaps buffer)))
                           (if (prefix-command-p cmd)
                               (progn
                                 (echo "~a-" (keys-description prefix-keys)))
                               (progn
                                 (let ((*this-command-keys* prefix-keys))
                                   (setq prefix-keys nil)
                                   (run-command buffer cmd))))
                           (progn
                             (echo "~a is undefined" (keys-description prefix-keys))
                             (setq prefix-keys nil))))))
                    ((equal type "load")
                     (with-current-buffer buffer
                       (let ((marker (focus-marker buffer)))
                         (setf (pos marker) (pos marker))
                         (dolist (style (styles buffer))
                           (update-style buffer style)))
                       (on-buffer-loaded buffer)))))
          (top-level ()
            (when recursive-p (error 'top-level)))
          (exit-recursive-edit (c)
            (when recursive-p
              (setq exit-condition (slot-value c 'condition))
              (return))))))
    (when exit-condition (error exit-condition))))

(defun recursive-edit ()
  (command-loop t))

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
