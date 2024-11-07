(in-package #:neomacs)

(define-mode minibuffer-search-mode (minibuffer-mode)
  ((for-buffer :initform (alex:required-argument :buffer)
               :initarg :buffer)))

(define-keys global
  "C-s" 'search-forward
  "C-r" 'search-backward)

(defun search-next-node (node query)
  (iter
    (when (text-node-p node)
      (when-let (offset (search (string-upcase query)
                                (string-upcase (text node))))
        (return (text-pos node offset))))
    (setq node (next-node node))
    (while node)))

(defun search-previous-node (node query)
  (iter
    (setq node (previous-node node))
    (when (text-node-p node)
      (when-let (offset (search (string-upcase query)
                                (string-upcase (text node))))
        (return (text-pos node offset))))
    (while node)))

(defmethod on-post-command progn ((buffer minibuffer-search-mode))
  (unless (member *this-command* '(search-forward search-backward))
    (let ((query (minibuffer-input buffer)))
      (when (plusp (length query))
        (with-current-buffer (for-buffer buffer)
          (or (when-let (next (search-next-node
                               (ensure-node (pos (focus)))
                               query))
                (let ((end (text-pos
                            (text-pos-node next)
                            (+ (text-pos-offset next)
                               (length query)))))
                  (setf
                   (pos (focus)) next
                   (pos (selection-marker (current-buffer))) end
                   (selection-active (current-buffer)) t))
                t)
              (message "No candidate")))))))

(defun start-search ()
  (with-marker (m (focus))
    (handler-case
        (unwind-protect
          (read-from-minibuffer
           "Search: " :modes 'minibuffer-search-mode
                      :buffer (current-buffer))
          (setf (selection-active (current-buffer)) nil))
      (quit () (setf (pos (focus)) (pos m))))))

(defun next-up-node (node)
  (next-node
   (or (and (element-p node) (last-child node))
       node)))

(defun ensure-node (pos)
  (ematch pos
    ((element) pos)
    ((text-pos node) node)
    ((end-pos node) (next-up-node node))))

(define-command search-forward ()
  (if (typep (current-buffer) 'minibuffer-search-mode)
      (let* ((buffer (current-buffer))
             (query (minibuffer-input buffer)))
        (when (plusp (length query))
          (with-current-buffer (for-buffer buffer)
            (or
             (when-let*
                 ((cur (ensure-node (pos (focus))))
                  (next (search-next-node (next-up-node cur) query)))
               (let ((end (text-pos
                           (text-pos-node next)
                           (+ (text-pos-offset next)
                              (length query)))))
                 (setf
                  (pos (focus)) next
                  (pos (selection-marker (current-buffer))) end
                  (selection-active (current-buffer)) t))
               t)
             (message "No previous candidate")))))
      (start-search)))

(define-command search-backward ()
  (if (typep (current-buffer) 'minibuffer-search-mode)
      (let* ((buffer (current-buffer))
             (query (minibuffer-input buffer)))
        (when (plusp (length query))
          (with-current-buffer (for-buffer buffer)
            (or
             (when-let*
                 ((cur (ensure-node (pos (focus))))
                  (prev (search-previous-node cur query)))
               (let ((end (text-pos
                           (text-pos-node prev)
                           (+ (text-pos-offset prev)
                              (length query)))))
                 (setf
                  (pos (focus)) prev
                  (pos (selection-marker (current-buffer))) end
                  (selection-active (current-buffer)) t))
               t)
             (message "No next candidate")))))
      (start-search)))
