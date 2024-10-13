(in-package #:neomacs)

(defun %quote-js (js-code)
  "Replace each backlash with 2, unless a \" follows it."
  (ppcre:regex-replace-all "\\\\(?!\")" js-code "\\\\\\\\"))

(defun evaluate-javascript (code buffer)
  (if buffer
      (cera.d:js
       cera.d:*driver*
       (ps:ps (ps:chain (ps:getprop (ps:chain -ceramic buffers) (ps:lisp (id buffer)))
                        web-contents
                        (execute-java-script (ps:lisp code) ps:false))))
      (cera.d:js cera.d:*driver* code)))

(defclass driver (ceramic.driver:driver) ())

(setq cera.d:*driver* (make-instance 'driver)
      trivial-ws:+default-timeout+ 1000000
      ceramic.setup::+main-javascript+ (asdf:system-relative-pathname :neomacs #p"main.js")
      ceramic.setup::*electron-version* "32.1.2")

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
