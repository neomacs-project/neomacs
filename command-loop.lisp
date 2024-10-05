(in-package #:neomacs)

(define-condition top-level () ()
  (:report "Return to top-level command loop"))

(define-condition exit-recursive-edit () ()
  (:report "Return from recursive edit"))

(defvar *event-queue* (sb-concurrency:make-mailbox))
(defvar *last-command* nil)
(defvar *this-command* nil)
(defvar *this-command-keys* nil)

(defun run-command (buffer command)
  (restart-case
      (let ((*adjust-marker-direction* *adjust-marker-direction*))
        (setf *last-command* *this-command*
              *this-command* command)
        (echo nil)
        (call-with-current-buffer buffer command))
    (abort ()
      :report "Return to command loop")))

(defun command-loop (&optional recursive-p)
  (iter (for data = (sb-concurrency:receive-message *event-queue*))
    (until (eql data 'quit))
    (for buffer = (gethash (parse-integer (assoc-value data :buffer)) *buffer-table*))
    (for event = (assoc-value data :input-event))
    (with prefix-keys = nil)
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
                     (if-let (cmd (lookup-keybind prefix-keys (all-keymaps buffer)))
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
                   (hooks:run-hook (buffer-loaded-hook buffer))))))
      (top-level ()
        (when recursive-p (error 'top-level)))
      (exit-recursive-edit ()
        (when recursive-p (return))))))

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
