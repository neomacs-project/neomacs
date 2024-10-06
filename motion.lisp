(in-package #:neomacs)

(defgeneric selectable-p-aux (buffer pos)
  (:method ((buffer buffer) (pos t)) t))

(defun selectable-p (pos)
  (selectable-p-aux (host pos) pos))

(defun word-character-p (neomacs node)
  (and (characterp node)
       (not (member node (word-boundary-list neomacs)))))

(defun word-start-p (pos)
  (and (word-character-p (host pos) (node-after pos))
       (not (word-character-p (host pos) (node-before pos)))))

(defun word-end-p (pos)
  (and (word-character-p (host pos) (node-before pos))
       (not (word-character-p (host pos) (node-after pos)))))

(define-condition motion-error (error) ())

(define-condition end-of-subtree (motion-error) ()
  (:report "End of subtree."))

(define-condition beginning-of-subtree (motion-error) ()
  (:report "Beginning of subtree."))

(define-condition top-of-subtree (motion-error) ()
  (:report "Top of subtree."))

(define-condition leaf-of-subtree (motion-error) ()
  (:report "Leaf of subtree."))

(define-command forward-node (&optional (marker (focus)))
  "Move to closest selectable preorder successor."
  (setf (pos marker)
        (or (npos-next-until (pos marker) #'selectable-p)
            (error 'top-of-subtree))))

(define-command backward-node (&optional (marker (focus)))
  "Move to closest selectable preorder predecessor."
  (setf (pos marker)
        (or (npos-prev-until (pos marker) #'selectable-p)
            (error 'top-of-subtree))))

(define-command forward-node-cycle (&optional (marker (focus)))
  "Like `forward-node', but may wrap around to beginning of buffer."
  (setf (pos marker)
        (or (npos-next-until (pos marker) #'selectable-p)
            (npos-next-until (pos-down (restriction (host marker)))
                             #'selectable-p)
            (error 'top-of-subtree))))

(define-command backward-node-cycle (&optional (marker (focus)))
  "Like `backward-node', but may wrap around to beginning of buffer."
  (setf (pos marker)
        (or (npos-prev-until (pos marker) #'selectable-p)
            (npos-prev-until (end-pos (restriction (host marker)))
                             #'selectable-p)
            (error 'top-of-subtree))))

(defun graphic-element-p (node)
  (and (element-p node)
       (not (new-line-node-p node))))

(define-command forward-element (&optional (marker (focus)))
  "Move after first element (exclude line break) to the right."
  (setf (pos marker)
        (or (iterate-pos-until
             (alex:disjoin #'npos-right
                           (alex:compose #'pos-right #'pos-up))
             (pos marker)
             (alex:compose #'graphic-element-p #'node-before))
            (error 'top-of-subtree))))

(define-command backward-element (&optional (marker (focus)))
  "Move to first element (exclude line break) to the left."
  (setf (pos marker)
        (or (iterate-pos-until
             (alex:disjoin #'npos-left #'pos-up)
             (pos marker)
             #'graphic-element-p)
            (error 'top-of-subtree)))
  (setq *adjust-marker-direction* 'backward))

(define-command beginning-of-buffer (&optional (marker (focus)))
  "Move to beginning of buffer."
  (setf (pos marker) (pos-down (restriction (host marker)))))

(define-command end-of-buffer (&optional (marker (focus)))
  "Move to end of buffer."
  (setq *adjust-marker-direction* 'backward)
  (setf (pos marker) (end-pos (restriction (host marker)))))

(defun ensure-selectable (marker &optional backward)
  (let ((pos (pos marker)))
    (unless (selectable-p pos)
      (setq pos (if backward
                    (npos-prev-until pos #'selectable-p)
                    (npos-next-until pos #'selectable-p)))
      (setf (pos marker) (or pos
                             (if backward
                                 (npos-prev-until
                                  (pos-down-last (restriction (host marker)))
                                  #'selectable-p)
                                 (npos-next-until
                                  (pos-down (restriction (host marker)))
                                  #'selectable-p))
                             (progn
                               (warn "Failed to ensure-selectable: ~a"
                                     (host marker))
                               (pos marker)))))))

(define-command backward-up-node (&optional (marker (focus)))
  "Move to closest selectable parent."
  (setf (pos marker) (or (pos-up-until (pos marker) #'selectable-p)
                         (error 'top-of-subtree))))

(define-command forward-word (&optional (marker (focus)))
  "Move to next word end position."
  (let ((pos (pos marker)))
    (setq pos (npos-next-until pos #'word-end-p))
    (setf (pos marker) (or pos (error 'top-of-subtree)))))

(define-command backward-word (&optional (marker (focus) non-interactive))
  "Move to previous word start position."
  (let ((pos (pos marker)))
    (setq pos (npos-prev-until pos #'word-start-p))
    (setf (pos marker) (or pos (error 'top-of-subtree)))
    (unless non-interactive
      (setq *adjust-marker-direction* 'backward))))

(defgeneric block-element-p-aux (buffer element)
  (:method ((buffer buffer) (element element))
    (member (tag-name element)
            '("tr" "address" "article" "aside" "blockquote" "canvas" "dd" "div" "dl" "dt" "fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5" "h6" "header" "hr" "li" "main" "nav" "noscript" "ol" "p" "pre" "section" "table" "tfoot" "ul" "video")
            :test 'equal)))

(defun block-element-p (element)
  (when (element-p element)
    (block-element-p-aux (host element) element)))

(defun line-start-p (pos)
  (or (new-line-node-p (node-before pos))
      (block-element-p (node-after pos))))

(defun line-end-p (pos)
  (or (new-line-node-p (node-after pos))
      (and (end-pos-p pos)
           (block-element-p (end-pos-node pos)))))

(define-command beginning-of-line (&optional (marker (focus)))
  "Move to beginning of line.

Also returns number of skipped selectable position, useful for
non-interactive use."
  (let ((pos (pos marker))
        (n 0))
    (iter (until (line-start-p pos))
      (when (selectable-p pos) (incf n))
      (setq pos (or (npos-prev pos) (return))))
    (setf (pos marker) (or pos (error 'top-of-subtree)))
    n))

(define-command end-of-line (&optional (marker (focus) non-interactive))
  "Move to end of line."
  (let ((pos (pos marker)))
    (iter (until (line-end-p pos))
      (setq pos (or (npos-next pos) (return))))
    (setf (pos marker) (or pos (error 'top-of-subtree)))
    (unless non-interactive
      (setq *adjust-marker-direction* 'backward))))

(define-command beginning-of-defun (&optional (marker (focus)))
  "Move to current or previous toplevel node."
  (let (moved)
    (handler-case
        (iter
          (backward-up-node marker)
          (setq moved t))
      (top-of-subtree ()))
    (unless moved
      (let ((pos (pos marker)))
        (setq pos (npos-left-until
                   pos (alex:compose #'not #'new-line-node-p)))
        (setf (pos marker) (or pos (error 'top-of-subtree)))))))

(define-command end-of-defun (&optional (marker (focus)))
  "Move to next toplevel node."
  (handler-case (iter (backward-up-node marker))
    (top-of-subtree ()))
  (let ((pos (pos marker)))
    (setq pos (npos-right-until
               pos (alex:compose #'not #'new-line-node-p
                                 #'node-after)))
    (setf *adjust-marker-direction* 'backward
          (pos marker) (or pos (error 'top-of-subtree)))))

(defun forward-node-same-line (marker n)
  "Move MARKER forward by N selectable positions or till end of line."
  (let ((pos (pos marker)))
    (iter (until (new-line-node-p (node-after pos)))
      (while (plusp n))
      (when (selectable-p pos) (decf n))
      (setq pos (or (npos-next pos) (return))))
    (setf (pos marker) pos)))

(define-command previous-line (&optional (n 1) (marker (focus)))
  "Move to N-th previous line.

Try to keep horizontal location approximately the same."
  (let ((i (beginning-of-line marker)))
    (dotimes (_ n)
      (backward-node marker)
      (beginning-of-line))
    (forward-node-same-line marker i)))

(define-command next-line (&optional (n 1) (marker (focus)))
  "Move to N-th next line.

Try to keep horizontal location approximately the same."
  (let ((i (with-marker (tmp marker)
             (beginning-of-line tmp))))
    (dotimes (_ n)
      (end-of-line marker)
      (forward-node marker))
    (forward-node-same-line marker i)))

(define-command scroll-up-command ()
  "Move up `scroll-lines'."
  (previous-line (scroll-lines (current-buffer))))

(define-command scroll-down-command ()
  "Move down `scroll-lines'."
  (next-line (scroll-lines (current-buffer))))

(define-keys global
  "arrow-right" 'forward-node
  "arrow-left" 'backward-node
  "M-arrow-right" 'forward-word
  "M-arrow-left" 'backward-word
  "arrow-down" 'next-line
  "arrow-up" 'previous-line
  "end" 'end-of-line
  "home" 'beginning-of-line)

(define-keys global
  "C-f" 'forward-node
  "C-b" 'backward-node
  "M-f" 'forward-word
  "M-b" 'backward-word
  "C-M-f" 'forward-element
  "C-M-b" 'backward-element
  "M-<" 'beginning-of-buffer
  "M->" 'end-of-buffer
  "C-a" 'beginning-of-line
  "C-e" 'end-of-line
  "M-a" 'beginning-of-defun
  "M-e" 'end-of-defun
  "C-n" 'next-line
  "C-p" 'previous-line
  "C-v" 'scroll-down-command
  "M-v" 'scroll-up-command)
