(in-package #:neomacs)

(sera:export-always
    '(*allow-recursive-debug*))

(define-mode debugger-mode (read-only-mode lisp-mode)
  ((environment
    :initform (alex:required-argument :environment)
    :initarg :environment)
   (mailbox :initform nil :initarg :mailbox)))

(define-keys debugger-mode
  "a" 'debugger-invoke-abort
  "c" 'debugger-invoke-continue
  "enter" 'debugger-invoke-restart
  "v" 'debugger-show-source
  "e" 'debugger-eval-in-frame)

(defmethod revert-buffer-aux ((buffer debugger-mode))
  (erase-buffer)
  (insert-nodes
   (end-pos (document-root buffer))
   (dom `(:dl
          (:dt ,(print-dom
                 (dissect:environment-condition
                  (environment buffer))))
          (:dd ,(princ-to-string
                 (dissect:environment-condition
                  (environment buffer)))))))
  (insert-nodes (end-pos (document-root buffer))
                (make-element "h1" :children (list "Restarts:")))
  (let ((tbody (make-element "tbody"))
        (*print-case* :downcase))
    (insert-nodes
     (end-pos (document-root buffer))
     (make-element
      "table" :class "restart-table" :children (list tbody)))
    (iter (for r in (dissect:environment-restarts
                     (environment buffer)))
      (for i from 0)
      (insert-nodes
       (end-pos tbody)
       (attach-presentation
        (dom `(:tr
               (:td :class "restart-name"
                    ,(format nil "~a. ~a" i (dissect:name r)))
               (:td ,(dissect:report r))))
        r)))
    (setf (pos (focus)) (pos-down tbody)))
  (insert-nodes (end-pos (document-root buffer))
                (make-element "h1" :children (list "Backtrace:")))
  (let ((tbody (make-element "tbody"))
        (*print-case* :downcase))
    (insert-nodes
     (end-pos (document-root buffer))
     (make-element
      "table" :class "backtrace-table" :children (list tbody)))
    (iter (for frame in (dissect:environment-stack
                         (environment buffer)))
      (for i from 0)
      (insert-nodes
       (end-pos tbody)
       (attach-presentation
        (dom `(:tr
               (:td :class "frame-number"
                    ,(format nil "~a." i))
               (:td
                ,(print-dom
                  (cons (dissect:call frame)
                        (dissect:args frame)))
                ,@(when-let (locals (dissect:locals frame))
                    `((:table
                       :class "locals-table"
                       (:tbody
                        ,@(iter (for (name . value) in locals)
                            (collect
                                `(:tr
                                  (:td ,(princ-to-string name))
                                  (:td ,(print-dom value))))))))))))
        frame)))))

(defun find-restart-by-name (name)
  (iter (for r in (dissect:environment-restarts
                   (environment (current-buffer))))
    (when (equal (symbol-name (dissect:name r)) name)
      (return r))))

(defun debugger-invoke (restart)
  (quit-buffer (current-buffer))
  (if-let (mailbox (mailbox (current-buffer)))
    (sb-concurrency:send-message mailbox (list restart))
    (dissect:invoke restart)))

(define-command debugger-invoke-abort
  :mode debugger-mode ()
  (if-let (r (find-restart-by-name "ABORT"))
    (debugger-invoke r)
    (user-error "No restart named abort")))

(define-command debugger-invoke-continue
  :mode debugger-mode ()
  (if-let (r (find-restart-by-name "CONTINUE"))
    (debugger-invoke r)
    (user-error "No restart named continue")))

(define-command debugger-invoke-restart
  :mode debugger-mode ()
  (debugger-invoke
   (presentation-at (focus) 'dissect:restart t)))

(define-command debugger-show-source
  :mode debugger-mode ()
  (let* ((frame (dissect::frame
                 (presentation-at (focus) 'dissect:call t)))
         (location (sb-di:frame-code-location frame))
         (source (sb-di::code-location-debug-source location))
         (namestring (sb-c::debug-source-namestring source)))
    (visit-source
     (when namestring (pathname namestring))
     (sb-di::code-location-toplevel-form-offset location)
     (sb-di::code-location-form-number location)
     (sb-c::debug-source-plist source))))

(defmethod current-package-aux ((buffer debugger-mode) pos)
  (or (when-let* ((call (presentation-at pos 'dissect:call))
                  (fun (dissect:call call))
                  (name (typecase fun
                          (symbol fun)
                          (function (sb-impl::%fun-name fun)))))
        (typecase name
          (symbol (symbol-package name))
          ((cons (eql setf) (cons symbol))
           (symbol-package (cadr name)))))
      (call-next-method)))

(define-command debugger-eval-in-frame
  :mode debugger-mode ()
  (let* ((frame (dissect::frame
                 (presentation-at (focus) 'dissect:call t)))
         (location (sb-di:frame-code-location frame))
         (*package* (current-package)))
    (message
     "~S"
     (funcall (sb-di:preprocess-for-eval
               (read-from-minibuffer
                (format nil "Eval in frame (~a): " (package-name *package*))
                :mode 'lisp-minibuffer-mode)
               location)
              frame))))

(defun debug-for-environment (env mailbox)
  (let ((debugger
          (make-buffer
           "*debugger*"
           :mode 'debugger-mode
           :environment env
           :mailbox mailbox
           :revert t)))
    (pop-to-buffer debugger)))

(defvar *allow-recursive-debug* nil
  "Whether to allow recursively enter the debugger.

If nil, turn off `*debugger-on-error*' inside debugger to avoid
recursive invocation, which can result in dead loop.")

(defun invoke-neomacs-debugger (c)
  (if (eq (bt:current-thread) *command-loop-thread*)
      (trivial-custom-debugger:with-debugger (#'neomacs-debugger-hook)
        (let ((*debug-on-error* *allow-recursive-debug*))
          (debug-for-environment
           (dissect:capture-environment c)
           nil)
          (recursive-edit)))
      (let ((mailbox (sb-concurrency:make-mailbox)))
        (sb-concurrency:send-message
         *event-queue*
         (list (cons
                :input-event
                (list (cons :type 'debug-request)
                      (cons :environment
                            (dissect:capture-environment c))
                      (cons :mailbox mailbox)))
               ;; Dummy buffer
               (cons :buffer "-1")))
        (apply #'dissect:invoke
               (sb-concurrency:receive-message mailbox)))))

(defun neomacs-debugger-hook (c hook)
  (declare (ignore hook))
  (if *debug-on-error*
      (invoke-neomacs-debugger c)
      (progn
        (funcall *error-hook* c)
        (message "Error in ~a: ~a" (bt2:current-thread) c)
        (sb-thread:abort-thread))))

(defsheet debugger-mode
    `(("table" :width "100vw"
               :border-collapse "collapse")
      (".restart-table td" :padding-right "1em")
      (".locals-table td" :padding-right "1em")
      ("p" :margin-top 0 :margin-bottom 0)
      ("h1" :margin-top 0 :margin-bottom 0
            :font-size "1.2rem")
      ("td"
       :white-space "nowrap"
       :vertical-align "top")))
