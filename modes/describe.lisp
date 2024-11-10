(in-package #:neomacs)

(define-mode describe-mode () ())

(push 'read-only-mode (hooks 'describe-mode))

(define-keys describe-mode
  "q" 'quit-buffer)

(defun read-key-sequence (prompt)
  (message "~a" prompt)
  (recursive-edit
   (constantly t) nil
   (lambda (cmd)
     (declare (ignore cmd))
     (message nil)
     (return-from read-key-sequence *this-command-keys*))))

(define-mode describe-key-mode
    (describe-mode)
  ((for-key :initform (alex:required-argument :key)
            :initarg :key)
   (for-buffer :initform (alex:required-argument :buffer)
               :initarg :buffer)))

(define-command describe-key
  :interactive
  (lambda ()
    (list (read-key-sequence "Describe the following key: ")))
  (keyseq)
  "Describe function bound to KEYSEQ in current buffer."
  (when keyseq
    (pop-to-buffer
     (make-buffer
      "*describe-key*" :mode 'describe-key-mode
      :key keyseq
      :buffer (current-buffer)
      :revert t))))

(defun lookup-keybind-trace (key buffer)
  (let (cmd trace)
    (iter (for mode in
               (cons :global
                     (reverse (sb-mop:class-precedence-list
                               (class-of buffer)))))
      (for keymap = (keymap mode))
      (unless keymap (next-iteration))
      (multiple-value-bind (next bind-type)
          (keymap-find-keybind keymap key cmd)
        (when bind-type
          (push (list next mode bind-type) trace))
        (setq cmd next)))
    (values cmd trace)))

(defmethod revert-buffer-aux ((buffer describe-key-mode))
  (erase-buffer)
  (multiple-value-bind (cmd trace)
      (lookup-keybind-trace (for-key buffer)
                            (for-buffer buffer))
    (if cmd
        (progn
          (insert-nodes
           (end-pos (document-root buffer))
           (dom `(:span
                  (:code ,(key-description (for-key buffer)))
                  " is bound to "
                  ,(print-dom cmd)
                  " in "
                  ,(print-dom (for-buffer buffer)))))
          (insert-nodes
           (end-pos (document-root buffer))
           (dom
            (cons :ul (iter (for (cmd mode bind-type) in trace)
                        (ecase bind-type
                          ((:key)
                           (collect
                               `(:li
                                 "bound to "
                                 ,(print-dom cmd)
                                 " by "
                                 ,(print-dom
                                   (if (eql mode :global)
                                       '*global-keymap*
                                       (or (class-name mode)
                                           mode))))))
                          ((:function)
                           (collect
                               `(:li
                                 "translated to "
                                 ,(print-dom cmd)
                                 " by "
                                 ,(print-dom
                                   (if (eql mode :global)
                                       '*global-keymap*
                                       (or (class-name mode)
                                           mode)))))))))))
          (if-let (doc (documentation cmd 'function))
            (let ((paragraphs (str:split "

"
                                         doc)))
              (iter (for p in paragraphs)
                (collect
                    (insert-nodes
                     (end-pos (document-root buffer))
                     (make-element
                      "p" :children
                      (render-doc-string-paragraph p))))))
            (insert-nodes
             (end-pos (document-root buffer))
             (make-element
              "p" :children
              (list "No docstring avaliable.")))))
        (progn
          (insert-nodes
           (end-pos (document-root buffer))
           (dom `(:p
                  (:code ,(key-description (for-key buffer)))
                  " is unbound in "
                  ,(print-dom (for-buffer buffer)))))))))

(defstyle describe-key-mode `(("ul" :margin-top 0)))
