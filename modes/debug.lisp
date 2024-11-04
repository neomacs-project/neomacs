(in-package #:neomacs)

(define-mode debugger-mode (read-only-mode lisp-mode)
  ((for-condition
    :initform (alex:required-argument :condition)
    :initarg :condition)
   (restarts
    :initform (alex:required-argument :restarts)
    :initarg :restarts)
   (stack
    :initform (alex:required-argument :stack)
    :initarg :stack)))

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
    (list (princ-to-string (for-condition buffer)))))
  (insert-nodes (end-pos (document-root buffer))
                (make-element "p" :children (list "Restarts:")))
  (let ((tbody (make-element "tbody"))
        (*print-case* :downcase))
    (insert-nodes
     (end-pos (document-root buffer))
     (make-element
      "table" :class "restart-table" :children (list tbody)))
    (iter (for r in (restarts buffer))
      (for i from 0)
      (insert-nodes
       (end-pos tbody)
       (lret ((el (make-element
                   "tr" :children
                   (list
                    (make-element
                     "td" :class "restart-name" :children
                     (list (format nil "~a. ~a" i (dissect:name r))))
                    (make-element
                     "td" :children
                     (list (dissect:report r)))))))
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
    (iter (for frame in (stack buffer))
      (for i from 0)
      (insert-nodes
       (end-pos tbody)
       (lret ((el (make-element
                   "tr" :children
                   (list
                    (make-element
                     "td" :class "frame-number" :children
                     (list (format nil "~a." i)))
                    (make-element
                     "td" :children
                     (list*
                      (print-dom
                       (cons (dissect:call frame)
                             (dissect:args frame)))
                      (when-let (locals (dissect:locals frame))
                        (list
                         (make-element
                          "table" :class "locals-table" :children
                          (list
                           (make-element
                            "tbody" :children
                            (iter (for (name . value) in locals)
                              (collect
                                  (make-element
                                   "tr" :children
                                   (list
                                    (make-element
                                     "td" :children
                                     (list (princ-to-string name)))
                                    (make-element
                                     "td" :children
                                     (list (print-dom value))))))))))))))))))
         (setf (attribute el 'frame) frame))))))

(defun find-restart-by-name (name)
  (iter (for r in (restarts (current-buffer)))
    (when (equal (symbol-name (dissect:name r)) name)
      (return r))))

(define-command debugger-invoke-abort
  :mode debugger-mode ()
  (if-let (r (find-restart-by-name "ABORT"))
    (dissect:invoke r)
    (user-error "No restart named abort")))

(define-command debugger-invoke-continue
  :mode debugger-mode ()
  (if-let (r (find-restart-by-name "CONTINUE"))
    (dissect:invoke r)
    (user-error "No restart named continue")))

(define-command debugger-invoke-restart
  :mode debugger-mode ()
  (let ((restart (when-let (row (focused-row))
                   (attribute row 'restart))))
    (unless restart
      (user-error "No restart under focus"))
    (dissect:invoke restart)))

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

(defun debug-for-condition (c)
  (let ((debugger
          (with-current-buffer
              (make-buffer
               "*debugger*"
               :modes '(debugger-mode)
               :condition c
               :restarts (dissect:restarts c)
               :stack (dissect:stack))
            (revert-buffer)
            (current-buffer))))
    (focus-buffer
     (display-buffer-right
      debugger))
    (unwind-protect
         (recursive-edit)
      (quit-buffer debugger))))

(defstyle debugger-mode
    `(("table" :width "100%"
               :border-collapse "collapse")
      (".restart-table td" :padding-right "1em")
      (".locals-table td" :padding-right "1em")
      ("p" :margin-bottom 0)
      ("td"
       :white-space "nowrap"
       :vertical-align "top")))
