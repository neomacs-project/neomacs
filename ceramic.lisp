(in-package #:neomacs)

(defun %quote-js (js-code)
  "Replace each backlash with 2, unless a \" follows it."
  (ppcre:regex-replace-all "\\\\(?!\")" js-code "\\\\\\\\"))

(defvar *force-sync-evaluate* nil)

(defun evaluate-javascript (code buffer)
  "Evaluate JavaScript CODE asynchronously.

Evaluate CODE in BUFFER's webContents, or main Electron process if
BUFFER is NIL. Returns NIL."
  (if buffer
      (cera.d:js
       cera.d:*driver*
       (ps:ps (ps:chain (js-buffer buffer) web-contents
                        (execute-java-script (ps:lisp code) t))))
      (cera.d:js cera.d:*driver* code))
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

(defclass driver (ceramic.driver:driver) ())

(setq cera.d:*driver* (make-instance 'driver)
      trivial-ws:+default-timeout+ 1000000
      ceramic.setup::+main-javascript+ (asdf:system-relative-pathname :neomacs #p"main.js")
      ceramic.setup::*electron-version* "33.0.2")

(defmethod ceramic.driver::on-message ((driver driver) message)
  (declare (type string message))
  (let ((data (cl-json:decode-json-from-string message)))
    (with-slots (cera.d::responses cera.d::js-lock cera.d::js-cond) driver
      (if (assoc-value data :id)
          (bt:with-lock-held (cera.d::js-lock)
            (setf (gethash (assoc-value data :id) cera.d::responses)
                  (assoc-value data :result))
            (bt:condition-notify cera.d::js-cond))
          (sb-concurrency:send-message *event-queue* data)))))

(define-command kill-neomacs ()
  "Exit Neomacs."
  ;; Mark all buffer as non-alive to suppress post-command operations
  (clrhash *buffer-table*)
  (sb-concurrency:send-message *event-queue* 'quit)
  (ceramic:quit))

(define-keys global
  "C-x C-c" 'kill-neomacs)
