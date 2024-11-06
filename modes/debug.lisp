(in-package #:neomacs)

(define-mode debugger-mode (read-only-mode lisp-mode)
  ((environment
    :initform (alex:required-argument :environment)
    :initarg :environment)))

(define-keys debugger-mode
  "a" 'debugger-invoke-abort
  "c" 'debugger-invoke-continue
  "enter" 'debugger-invoke-restart
  "v" 'debugger-show-source)

(defmethod revert-buffer-aux ((buffer debugger-mode))
  (erase-buffer)
  (insert-nodes
   (end-pos (document-root buffer))
   (make-element
    "p" :children
    (list (princ-to-string
           (dissect:environment-condition
            (environment buffer))))))
  (insert-nodes (end-pos (document-root buffer))
                (make-element "p" :children (list "Restarts:")))
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
       (lret ((el (dom `(:tr
                         (:td :class "restart-name"
                              ,(format nil "~a. ~a" i (dissect:name r)))
                         (:td ,(dissect:report r))))))
         (setf (attribute el 'restart) r))))
    (setf (pos (focus)) (pos-down tbody)))
  (insert-nodes (end-pos (document-root buffer))
                (make-element "p" :children (list "Backtrace:")))
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
       (lret ((el
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
                                         (:td ,(print-dom value))))))))))))))
         (setf (attribute el 'frame) frame))))))

(defun find-restart-by-name (name)
  (iter (for r in (dissect:environment-restarts
                   (environment (current-buffer))))
    (when (equal (symbol-name (dissect:name r)) name)
      (return r))))

(defun debugger-invoke (restart)
  (quit-buffer (current-buffer))
  (dissect:invoke restart))

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
  (let ((restart (when-let (row (focused-row))
                   (attribute row 'restart))))
    (unless restart
      (user-error "No restart under focus"))
    (debugger-invoke restart)))

(define-command debugger-show-source
  :mode debugger-mode ()
  (let ((frame (when-let (row (focused-row))
                 (attribute row 'frame))))
    (unless frame
      (user-error "No frame under focus"))
    (let* ((frame (dissect::frame frame))
           (location (sb-di:frame-code-location frame))
           (source (sb-di::code-location-debug-source location))
           (namestring (sb-c::debug-source-namestring source)))
      (visit-source
       (when namestring (pathname namestring))
       (sb-di::code-location-toplevel-form-offset location)
       (sb-di::code-location-form-number location)
       (sb-c::debug-source-plist source)))))

(defun debug-for-environment (env)
  (let ((debugger
          (with-current-buffer
              (make-buffer
               "*debugger*"
               :modes '(debugger-mode)
               :environment env)
            (revert-buffer)
            (current-buffer))))
    (focus-buffer
     (display-buffer-right
      debugger))))

(defstyle debugger-mode
    `(("table" :width "100%"
               :border-collapse "collapse")
      (".restart-table td" :padding-right "1em")
      (".locals-table td" :padding-right "1em")
      ("p" :margin-bottom 0)
      ("td"
       :white-space "nowrap"
       :vertical-align "top")))
