(in-package #:neomacs)

(define-mode debugger-mode (read-only-mode)
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
  "a" 'debugger-invoke-abort)

(defmethod revert-buffer-aux ((buffer debugger-mode))
  (erase-buffer)
  (insert-nodes
   (end-pos (document-root buffer))
   (make-element
    "p" :children
    (list (princ-to-string (for-condition buffer)))))
  (let ((tbody (make-element "tbody")))
    (insert-nodes
     (end-pos (document-root buffer))
     (make-element
      "table" :class "restart-table" :children (list tbody)))
    (iter (for r in (restarts buffer))
      (for i from 1)
      (insert-nodes
       (end-pos tbody)
       (make-element
        "tr" :children
        (list
         (make-element
          "td" :class "restart-name" :children
          (list (format nil "~a. ~a" i (dissect:name r))))
         (make-element
          "td" :children
          (list (dissect:report r)))))))
    (setf (pos (focus)) (pos-down tbody)))
  )

(define-command debugger-invoke-abort
  :mode debugger-mode ()
  (iter (for r in (restarts (current-buffer)))
    (when (member (symbol-name (dissect:name r))
                  '("ABORT" "ABORT*")
                  :test 'equal)
      (dissect:invoke r))))

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
      ("td" :padding-right "1em")
      (".restart-name" :white-space "nowrap")))
