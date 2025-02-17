(in-package #:neomacs)

(sera:export-always
    '(evaluate-javascript evaluate-javascript-sync
      quote-js kill-neomacs))

(defun quote-js (string)
  (with-output-to-string (s)
    (loop for char across string do
      (sera:cond-let it
        ((getf ps::*js-lisp-escaped-chars* char)
         (write-char #\\ s)
         (write-char it s))
        ((or (<= (char-code char) #x1F)
             (<= #x80 (char-code char) #x9F)
             (member (char-code char) '(#xA0 #xAD #x200B #x200C)))
         (format s "\\u~:@(~4,'0x~)" (char-code char)))
        (t
         (write-char char s))))))

(defvar *force-sync-evaluate* nil)

(defun send-js-for-buffer (code buffer)
  (cera.d:js
   cera.d:*driver*
   (format nil "Ceramic.buffers[~S].webContents.executeJavaScript('~A',true)" (id buffer) (quote-js code))))

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
      (format nil "return Ceramic.buffers[~S].webContents.executeJavaScript('~A',true)" (id buffer) (quote-js code)))
     ((eql :global)
      (format nil "return eval('~A')" (quote-js code))))))

(defclass driver (ceramic.driver:driver) ())

;; Don't run this when reloading neomacs system
(unless (typep cera.d:*driver* 'driver)
  (setq cera.d:*driver* (make-instance 'driver)
        trivial-ws:+default-timeout+ 1000000
        ceramic.setup::+main-javascript+ (asdf:system-relative-pathname :neomacs #p"main.js")
        ceramic.setup::*electron-version* "34.2.0"))

(defvar *event-queue* (sb-concurrency:make-mailbox))

(defvar *command-loop-thread* nil)

(defvar *last-quit-time* nil)

(defvar *quit-interrupt-threshold* 0.2
  "Duration in seconds to trigger a quit interrupt.

Interrupt the command loop if it has not responded to a quit event for
this long.")

(defun quit-event-p (message)
  (let ((event (assoc-value message :input-event)))
    (and (equal (assoc-value event :type) "keyDown")
         (equal (assoc-value event :key) "g")
         (assoc-value event :control)
         (not (assoc-value event :alt))
         (not (assoc-value event :meta)))))

(defmethod ceramic.driver::on-message ((driver driver) message)
  (declare (type string message))
  (let ((data (cl-json:decode-json-from-string message)))
    (with-slots (cera.d::responses cera.d::js-cond cera.d::js-lock) driver
      (if-let (id (assoc-value data :id))
        (bt:with-lock-held (cera.d::js-lock)
          (setf (gethash id cera.d::responses)
                (assoc-value data :result))
          (bt2:condition-broadcast cera.d::js-cond))
        (progn
          (when (quit-event-p data)
            (let ((interrupt-p
                    (and *last-quit-time*
                         (> (- (get-internal-real-time) *last-quit-time*)
                            (* internal-time-units-per-second *quit-interrupt-threshold*)))))
              (setf *last-quit-time* (get-internal-real-time))
              (when interrupt-p
                (sb-thread:interrupt-thread
                 *command-loop-thread* (lambda () (signal 'async-quit))))))
          (sb-concurrency:send-message *event-queue* data))))))

(ceramic.resource:define-resources :neomacs ()
  (assets #p"assets/"))

(ceramic.resource:define-resources :neomacs ()
  (doc #p"doc/"))

(defun ceramic.runtime:executable-relative-pathname (pathname)
  "Return an absolute pathname relative to the executable pathname."
  (merge-pathnames pathname
                   (make-pathname
                    :name nil :type nil :version nil
                    :defaults
                    (ceramic.runtime:executable-pathname))))

(defun mount-asset (host path)
  "Mount files under PATH to neomacs://HOST/.

PATH should be an absolute directory pathname (ending with slash)."
  (evaluate-javascript
   (format nil "Mounts[~s]='~a'"
           host (quote-js (uiop:native-namestring path)))
   :global))
