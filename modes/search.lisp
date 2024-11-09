(in-package #:neomacs)

(define-mode minibuffer-search-mode (minibuffer-mode)
  ((for-buffer :initform (alex:required-argument :buffer)
               :initarg :buffer)))

(define-keys :global
  "C-s" 'search-forward
  "C-r" 'search-backward)

(defun search-text (query text &key (start 0) (from-end nil) (end nil))
  "Search for QUERY inside TEXT."
  (when (< start (length text))
    (search (string-upcase query) (string-upcase text)
            :start2 start :end2 end :from-end from-end)))

(defun search-next-node (node query)
  "Search for next occurrence of QUERY, start from beginning of NODE."
  (iter
    (when (text-node-p node)
      (when-let (offset (search-text query (text node)))
        (return (text-pos node offset))))
    (setq node (next-node node))
    (while node)))

(defun search-previous-node (node query)
  "Search for previous occurrence of QUERY, start from beginning of NODE."
  (iter
    (setq node (previous-node node))
    (when (text-node-p node)
      (when-let (offset (search-text query (text node) :from-end t))
        (return (text-pos node offset))))
    (while node)))

(defun search-next-pos (pos query &optional must-move)
  "Search for next occurence of QUERY, start from POS.

If MUST-MOVE is true, always return a different POS."
  (ematch pos
    ((element) (search-next-node pos query))
    ((text-pos node offset)
     (if-let (beg (search-text query (text node)
                               :start (+ offset (if must-move (length query) 0))))
       (text-pos node beg)
       (search-next-node (next-node node) query)))
    ((end-pos node) (search-next-node node query))))

(defun search-previous-pos (pos query)
  "Search for previous occurence of QUERY, start from POS."
  (ematch pos
    ((element) (search-previous-node pos query))
    ((text-pos node offset)
     (if-let (beg (search-text query (text node) :from-end t :end offset))
       (text-pos node beg)
       (search-previous-node (previous-node node) query)))
    ((end-pos node) (search-previous-node (next-up-node node) query))))

(defmethod on-post-command progn ((buffer minibuffer-search-mode))
  (unless (member *this-command* '(search-forward search-backward))
    (let ((query (minibuffer-input buffer)))
      (when (plusp (length query))
        (with-current-buffer (for-buffer buffer)
          (or (when-let (next (search-next-pos (pos (focus)) query))
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

(define-command search-forward ()
  "Search forward for text occurrence."
  (if (typep (current-buffer) 'minibuffer-search-mode)
      (let* ((buffer (current-buffer))
             (query (minibuffer-input buffer)))
        (when (plusp (length query))
          (with-current-buffer (for-buffer buffer)
            (or
             (when-let ((next (search-next-pos (pos (focus)) query t)))
               (let ((end (text-pos
                           (text-pos-node next)
                           (+ (text-pos-offset next)
                              (length query)))))
                 (setf
                  (pos (focus)) next
                  (pos (selection-marker (current-buffer))) end
                  (selection-active (current-buffer)) t))
               t)
             (message "No next candidate")))))
      (start-search)))

(define-command search-backward ()
  "Search backward for text occurrence."
  (if (typep (current-buffer) 'minibuffer-search-mode)
      (let* ((buffer (current-buffer))
             (query (minibuffer-input buffer)))
        (when (plusp (length query))
          (with-current-buffer (for-buffer buffer)
            (or
             (when-let ((prev (search-previous-pos (pos (focus)) query)))
               (let ((end (text-pos
                           (text-pos-node prev)
                           (+ (text-pos-offset prev)
                              (length query)))))
                 (setf
                  (pos (focus)) prev
                  (pos (selection-marker (current-buffer))) end
                  (selection-active (current-buffer)) t))
               t)
             (message "No previous candidate")))))
      (start-search)))
