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
  "enter" 'debugger-invoke-restart)

(defmethod revert-buffer-aux ((buffer debugger-mode))
  (erase-buffer)
  (insert-nodes
   (end-pos (document-root buffer))
   (make-element
    "p" :children
    (list (princ-to-string (for-condition buffer)))))
  (insert-nodes (end-pos (document-root buffer))
                (make-element "p" :children (list "Restarts:")))
  (let ((tbody (make-element "tbody")))
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
  (let ((tbody (make-element "tbody")))
    (insert-nodes
     (end-pos (document-root buffer))
     (make-element
      "table" :class "backtrace-table" :children (list tbody)))
    (iter (for frame in (stack buffer))
      (for i from 0)
      (insert-nodes
       (end-pos tbody)
       (make-element
        "tr" :children
        (list
         (make-element
          "td" :class "frame-number" :children
          (list (format nil "~a." i)))
         (make-element
          "td" :children
          (list (print-dom
                 (cons (dissect:call frame)
                       (dissect:args frame)))))))))))

(defun find-restart-by-name (name)
  (iter (for r in (restarts (current-buffer)))
    (when (equal (symbol-name (dissect:name r)) name)
      (return r))))

(define-command debugger-invoke-abort
  :mode debugger-mode ()
  (if-let (r (find-restart-by-name "ABORT"))
    (dissect:invoke r)
    (message "No restart named abort")))

(define-command debugger-invoke-continue
  :mode debugger-mode ()
  (if-let (r (find-restart-by-name "CONTINUE"))
    (dissect:invoke r)
    (message "No restart named continue")))

(define-command debugger-invoke-restart
  :mode debugger-mode ()
  (or (when-let (restart (when-let (row (focused-row))
                           (attribute row 'restart)))
        (dissect:invoke restart)
        t)
      (message "No restart under focus")))

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
      ("p" :margin-bottom 0)
      (".restart-name, .frame-number"
       :white-space "nowrap"
       :vertical-align "top")))
