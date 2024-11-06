(in-package #:neomacs)

(defun %quote-js (js-code)
  "Replace each backlash with 2, unless a \" follows it."
  (ppcre:regex-replace-all "\\\\(?!\")" js-code "\\\\\\\\"))

(defvar *force-sync-evaluate* nil)

(defun evaluate-javascript (code buffer)
  "Evaluate JavaScript CODE asynchronously.

Evaluate CODE in BUFFER's webContents, or main Electron process if
BUFFER is NIL. Returns NIL."
  (if *force-sync-evaluate*
      (evaluate-javascript-sync code buffer)
      (if buffer
          (cera.d:js
           cera.d:*driver*
           (ps:ps (ps:chain (js-buffer buffer) web-contents
                            (execute-java-script (ps:lisp code) t))))
          (cera.d:js cera.d:*driver* code)))
  nil)

(defun evaluate-javascript-sync (code buffer)
  "Evaluate JavaScript CODE synchronously and return the result.

Evaluate CODE in BUFFER's webContents, or main Electron process if
BUFFER is NIL."
  (if buffer
      (cera.d:sync-js
       cera.d:*driver*
       (ps:ps
         (return
           (ps:chain (js-buffer buffer) web-contents
                     (execute-java-script (ps:lisp code) t)))))
      (cera.d:sync-js
       cera.d:*driver*
       (ps:ps (return (eval (ps:lisp code)))))))

(defun evaluate-javascript-async (code buffer cb)
  (if cb
      (let ((message-id (uuid:format-as-urn nil (uuid:make-v4-uuid))))
        (with-slots (callbacks cera.d::js-lock) cera.d:*driver*
          (bt:with-lock-held (cera.d::js-lock)
            (setf (gethash message-id callbacks) cb))
          (if buffer
              (cera.d:js
               cera.d:*driver*
               (ps:ps (ps:chain -ceramic
                                (sync-eval
                                 (ps:lisp message-id)
                                 (lambda ()
                                   (ps:chain (js-buffer buffer) web-contents
                                             (execute-java-script (ps:lisp code) t)))))))
              (cera.d:js
               cera.d:*driver*
               (ps:ps (ps:chain -ceramic
                                (sync-eval
                                 (ps:lisp message-id)
                                 (lambda ()
                                   (eval (ps:lisp code))))))))))

      (evaluate-javascript-sync code buffer)))

(defclass driver (ceramic.driver:driver)
  ((callbacks
    :initform (make-hash-table :test 'equal))))

(setq cera.d:*driver* (make-instance 'driver)
      trivial-ws:+default-timeout+ 1000000
      ceramic.setup::+main-javascript+ (asdf:system-relative-pathname :neomacs #p"main.js")
      ceramic.setup::*electron-version* "33.0.2")

(defmethod ceramic.driver::on-message ((driver driver) message)
  (declare (type string message))
  (let ((data (cl-json:decode-json-from-string message)))
    (with-slots (cera.d::responses cera.d::js-lock
                 cera.d::js-cond callbacks)
        driver
      (if-let (id (assoc-value data :id))
          (when-let
              (cb (bt:with-lock-held (cera.d::js-lock)
                    (lret ((cb (gethash id callbacks)))
                      (if cb (remhash id callbacks)
                          (progn
                            (setf (gethash id cera.d::responses)
                                  (assoc-value data :result))
                            (bt:condition-notify cera.d::js-cond))))))
            (handler-case
                (funcall cb (assoc-value data :result))
              (error (c)
                (message "Error running callback ~a in JS communication thread: ~a"
                         cb c))))
          (sb-concurrency:send-message *event-queue* data)))))

(define-command kill-neomacs ()
  "Exit Neomacs."
  ;; Mark all buffer as non-alive to suppress post-command operations
  (clrhash *buffer-table*)
  (sb-concurrency:send-message *event-queue* 'quit)
  (ceramic:quit))

(define-keys global
  "C-x C-c" 'kill-neomacs)
