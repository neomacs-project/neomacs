(in-package #:neomacs)

(sera:export-always
    '(evaluate-javascript evaluate-javascript-sync))

(defun quote-js (js-code)
  "Replace each backslash with 2 (unless a \" follows it) and escape backquotes."
  (ppcre:regex-replace-all "`"
                           (ppcre:regex-replace-all "\\\\(?!\")" js-code "\\\\\\\\")
                           "\\\\`"))

(defun asset-url (relative-pathname)
  (str:concat
   "file://"
   (uiop:native-namestring
    (ceramic:resource
     'assets relative-pathname))))

(defvar *force-sync-evaluate* nil)

(defun send-js-for-buffer (code buffer)
  (cera.d:js
   cera.d:*driver*
   (format nil "Ceramic.buffers[~S].webContents.executeJavaScript(`~A`,true)" (id buffer) (quote-js code))))

(sera:-> evaluate-javascript (string (or buffer (eql :global))) null)
(defun evaluate-javascript (code buffer)
  "Evaluate JavaScript CODE asynchronously.

Evaluate CODE in BUFFER's webContents, or main Electron process if
BUFFER is :global. Returns NIL."
  (if *force-sync-evaluate*
      (evaluate-javascript-sync code buffer)
      (etypecase buffer
        (buffer
         (if (amalgamate-js-p buffer)
             (progn
               (write-string code (amalgamate-js-stream buffer))
               (terpri (amalgamate-js-stream buffer)))
             (send-js-for-buffer code buffer)))
        ((eql :global)
         (cera.d:js cera.d:*driver* code))))
  nil)

(sera:-> evaluate-javascript-sync (string (or buffer (eql :global))) t)
(defun evaluate-javascript-sync (code buffer)
  "Evaluate JavaScript CODE synchronously and return the result.

Evaluate CODE in BUFFER's webContents, or main Electron process if
BUFFER is NIL."
  (cera.d:sync-js
   cera.d:*driver*
   (etypecase buffer
     (buffer
      (format nil "return Ceramic.buffers[~S].webContents.executeJavaScript(`~A`,true)" (id buffer) (quote-js code)))
     ((eql :global)
      (format nil "return eval(`~A`)" (quote-js code))))))

(defclass driver (ceramic.driver:driver) ())

(setq cera.d:*driver* (make-instance 'driver)
      trivial-ws:+default-timeout+ 1000000
      ceramic.setup::+main-javascript+ (asdf:system-relative-pathname :neomacs #p"main.js")
      ceramic.setup::*electron-version* "33.0.2")

(defmethod ceramic.driver::on-message ((driver driver) message)
  (declare (type string message))
  (let ((data (cl-json:decode-json-from-string message)))
    (with-slots (cera.d::responses cera.d::js-cond cera.d::js-lock) driver
      (if-let (id (assoc-value data :id))
        (bt:with-lock-held (cera.d::js-lock)
          (setf (gethash id cera.d::responses)
                (assoc-value data :result))
          (bt2:condition-broadcast cera.d::js-cond))
        (sb-concurrency:send-message *event-queue* data)))))

(ceramic.resource:define-resources :neomacs ()
  (assets #p"assets/"))

(define-command kill-neomacs ()
  "Exit Neomacs."
  (save-web-history)
  ;; Mark all buffer as non-alive to suppress post-command operations
  (clrhash *buffer-table*)
  (sb-concurrency:send-message *event-queue* 'quit)
  (ceramic:quit))

(define-keys :global
  "C-x C-c" 'kill-neomacs)
