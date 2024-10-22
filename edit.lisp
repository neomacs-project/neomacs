(in-package :neomacs)

;;; DOM Edit

(defun assign-neomacs-id (node)
  (setf (attribute node "neomacs-identifier")
        (princ-to-string (incf (next-neomacs-id (current-buffer)))))
  node)

(defun text-markers-in (neomacs text-node offset length)
  (iter (for m in (markers neomacs))
    (labels ((process-text-pos (pos)
               (when (eq (text-pos-node pos) text-node)
                 (when (and (>= (text-pos-offset pos) offset)
                            (or (not length)
                                (> (+ offset length)
                                   (text-pos-offset pos))))
                   (collect m)))))
      (match (slot-value m 'pos)
        ((and (text-pos) pos)
         (process-text-pos pos))
        ((%after-pos before)
         (when (text-pos-p before)
           (process-text-pos before)))))))

(defun move-text-markers
    (neomacs src-node src-offset dst-node dst-offset length)
  "Move markers in NEOMACS pointing inside text node.

Move markers pointing inside [SRC-OFFSET,SRC-OFFSET+LENGTH) of
SRC-NODE to [DST-OFFSET,DST-OFFSET+LENGTH) of DST-NODE.
If LENGTH is NIL, move everything after SRC-OFFSET."
  (check-type src-node text-node)
  (let ((ms (text-markers-in neomacs src-node src-offset length)))
    (dolist (m ms)
      (labels ((process-text-pos (pos)
                 (setf (text-pos-node pos) dst-node
                       (text-pos-offset pos)
                       (+ dst-offset (- (text-pos-offset pos) src-offset)))))
        (match (slot-value m 'pos)
          ((and (text-pos) pos)
           (process-text-pos pos))
          ((%after-pos before)
           (process-text-pos before)))
        (when (eq m (focus-marker (host m)))
          (on-focus-move (host m) (pos m) (pos m)))))))

(defun merge-text-nodes (prev node)
  (let ((host (host node))
        (parent (parent node))
        (offset (length (text prev))))
    (send-dom-update
     `(let* ((node (js-node ,node))
             (prev (ps:chain node previous-sibling))
             (parent (js-node ,parent)))
        (ps:chain prev (append-data (ps:chain node data)))
        (ps:chain parent (remove-child node))
        nil)
     (host node))

    (setf (text prev)
          (sera:concat (text prev) (text node)))
    (remove-node node)
    (move-text-markers
     host node 0 prev offset nil)
    (record-undo
     (nclo undo-merge-text ()
       (split-text-node prev offset node))
     (nclo redo-merge-text ()
       (merge-text-nodes prev node)))))

(defun maybe-merge-text-nodes (node)
  (when node
    (let ((prev (previous-sibling node)))
      (when (and (text-node-p prev) (text-node-p node))
        (merge-text-nodes prev node)))))

(defun split-text-node (node offset next)
  (let ((parent (parent node))
        (host (host node)))
    (send-dom-update
     `(ps:chain (js-node ,node) (split-text ,offset))
     (host node))
    (insert-before parent next (next-sibling node))
    (psetf (text node) (subseq (text node) 0 offset)
           (text next) (subseq (text node) offset))
    (move-text-markers host node offset next 0 nil)
    (record-undo
     (nclo undo-split-text ()
       (merge-text-nodes node next))
     (nclo redo-split-text ()
       (split-text-node node offset next)))))

(defun maybe-split-text-node (pos)
  "Split `text-node' at POS if possible.

Returns the node after the position after this operation."
  (match pos
    ((text-pos node offset)
     (if (= offset 0)
         node
         (let ((next (make-instance 'text-node
                                    :text ""
                                    :host (host pos))))
           (split-text-node node offset next)
           next)))
    (_ (node-after pos))))

(defun insert-nodes-2 (parent nodes reference)
  (send-dom-update
   `(let* ((parent (js-node ,parent))
           (reference (js-node ,reference))
           (template (ps:chain document (create-element "template"))))
      (setf (ps:chain template inner-h-t-m-l)
            ,(with-output-to-string (stream)
               (dolist (c nodes)
                 (serialize c stream))))
      (ps:chain -array
                (from (ps:chain template content child-nodes))
                (for-each (lambda (c)
                            (ps:chain parent (insert-before c reference)))))
      nil)
   (host parent))

  (dolist (c nodes)
    (insert-before parent c reference))

  (record-undo
   (nclo undo-insert-nodes ()
     (delete-nodes-2 parent (car nodes) reference))
   (nclo redo-insert-nodes ()
     (insert-nodes-2 parent nodes reference))))

(defun insert-nodes-1 (pos nodes)
  "Internal function for inserting NODES."
  (bind ((parent (node-containing pos))
         (reference (maybe-split-text-node pos)))
    (insert-nodes-2 parent nodes reference)
    (maybe-merge-text-nodes (car nodes))
    (maybe-merge-text-nodes reference)
    nil))

(defun node-setup (node host)
  "Setup NODE as a good citizen of HOST.

This assigns a neomacs-id attribute and run `on-node-setup'.

This function should be called on all nodes entering HOST's DOM
tree (which is usually taken care of by `insert-nodes')."
  (setf (host node) host)
  (when (element-p node)
    (assign-neomacs-id node)
    (on-node-setup host node)))

(defun insert-nodes (marker-or-pos &rest things)
  "Insert THINGS at MARKER-OR-POS.

THINGS can be DOM nodes or strings, which are converted to text nodes."
  (let* ((pos (resolve-marker marker-or-pos))
         (host (host pos)))
    (unless host
      (error "~a does not point inside an active document." pos))
    (check-read-only host)
    (let ((nodes
            ;; TODO: do more cleanup, like merging adjacent text nodes
            (iter (for n in things)
              (if (stringp n)
                  (when (> (length n) 0)
                    (collect (make-instance 'text-node :text n)))
                  (collect n)))))
      (record-undo
       (nclo undo-node-setup ()
         (mapc (alex:curry #'do-dom #'node-cleanup) nodes))
       (nclo redo-node-setup ()
         (mapc (alex:curry #'do-dom (alex:rcurry #'node-setup host)) nodes)))
      (insert-nodes-1 pos (mapc (alex:curry #'do-dom (alex:rcurry #'node-setup host))
                                nodes)))))

(defun count-nodes-between (beg end)
  (iter (for node first beg then (next-sibling node))
    (while node)
    (until (eql node end))
    (sum 1)))

(defun delete-nodes-2 (parent beg end)
  (let ((reference (previous-sibling beg))
        (length (count-nodes-between beg end)))
    (send-dom-update
     (if reference
         `(let ((parent (js-node ,parent))
                (reference (js-node ,reference)))
            (dotimes (_ ,length)
              (ps:chain parent (remove-child
                                (ps:chain reference next-sibling))))
            nil)
         `(let ((parent (js-node ,parent)))
            (dotimes (_ ,length)
              (ps:chain parent (remove-child
                                (ps:chain parent first-child))))
            nil))
     (host parent))
    (let ((nodes
            (iter (for node = (if reference (next-sibling reference)
                                  (first-child parent)))
              (while node)
              (until (eql node end))
              (remove-node node)
              (collect node))))
      (relocate-markers (host parent) nodes
                        (or (normalize-node-pos end nil)
                            (end-pos (parent beg))))
      (record-undo
       (nclo undo-delete-nodes ()
         (insert-nodes-2 parent nodes end))
       (nclo redo-delete-nodes ()
         (delete-nodes-2 parent beg end)))
      nodes)))

(defun relocate-markers (host deleted-nodes end)
  (labels ((node (marker-or-pos)
             (ematch marker-or-pos
               ((marker pos) (node pos))
               ((element) marker-or-pos)
               ((text-pos node) node)
               ((end-pos node) node)
               ((%start-pos node) node)
               ((%after-pos before) (node before)))))
    (dolist (n deleted-nodes)
      (do-dom (lambda (deleted-node)
                (dolist (m (markers host))
                  (when (eq (node m) deleted-node)
                    #+nil (let ((pos (pos m))
                                (mp (trivial-garbage:make-weak-pointer m)))
                            (record-undo
                             (nclo undo-move-marker ()
                               (when-let (m (trivial-garbage:weak-pointer-value mp))
                                 (setf (pos m) pos)))
                             (nclo redo-move-marker ()
                               (when-let (m (trivial-garbage:weak-pointer-value mp))
                                 (setf (pos m) end)))))
                    (setf (pos m) end))))
        n))))

(defun delete-nodes-1 (beg end)
  (let ((parent (node-containing beg)))
    (when end
      (unless (eql parent (node-containing end))
        (error "~a and ~a are not siblings." beg end)))
    (let* (;; Quirk: `maybe-split-text-node' may invalidate `text-pos'
           ;; after it. A correct way to handle this is to
           ;; use `marker' instead of `text-pos', which might be costly.
           ;; Currently we rely on BEG is before END.
           ;; (However, how do we make `move-nodes-2' correct?
           ;; TODO: sort positions before spliting.
           (end (maybe-split-text-node end))
           (beg (maybe-split-text-node beg))
           (nodes (delete-nodes-2 parent beg end)))

      (maybe-merge-text-nodes end)
      nodes)))

(defun node-cleanup (node)
  "Release resources associated with NODE under its active document.

This runs `on-node-cleanup' and removes any observers on NODE's cell
slots.

This function should be called on all nodes leaving HOST's DOM
tree (which is usually taken care of by `delete-nodes' and
`extract-nodes')."
  (on-node-cleanup (host node) node)
  (when (element-p node)
    (iter (for s in '(parent next-sibling previous-sibling
                      first-child last-child))
      (for c = (slot-value node s))
      (iter
        (for o in (lwcells::cell-outs c))
        (when (observer-cell-p o)
          (cell-set-function o nil)))))
  (setf (host node) nil))

(defun delete-nodes-0 (beg end)
  (let* ((beg (resolve-marker beg))
         (host (host beg))
         (end (resolve-marker end)))
    (unless host
      (error "~a does not point inside an active document." beg))
    (check-read-only host)
    ;; Account for this edge case
    (unless (or (end-pos-p beg) (equalp beg end))
      (let ((nodes (delete-nodes-1 beg end)))
        (mapc (alex:curry #'do-dom #'node-cleanup)
              nodes)
        (record-undo
         (nclo undo-node-cleanup ()
           (mapc (alex:curry #'do-dom (alex:rcurry #'node-setup host)) nodes))
         (nclo redo-node-cleanup ()
           (mapc (alex:curry #'do-dom #'node-cleanup) nodes)))
        nodes))))

(defun delete-nodes (beg end)
  "Delete nodes between BEG and END and returns nil.

BEG and END must be sibling positions.  If END is nil, delete children
starting from BEG till the end of its parent."
  (delete-nodes-0 beg end)
  nil)

(defun extract-nodes (beg end)
  "Like `delete-nodes', but clone and return the deleted contents."
  (mapcar #'clone-node (delete-nodes-0 beg end)))

(defun move-nodes-2 (src-parent beg end dst-parent reference)
  (let ((src-reference (previous-sibling beg))
        (length (count-nodes-between beg end)))

    (send-dom-update
     (if src-reference
         `(let ((src-reference (js-node ,src-reference))
                (dst-parent (js-node ,dst-parent))
                (dst-reference (js-node ,reference)))
            (dotimes (_ ,length)
              (ps:chain dst-parent
                        (insert-before
                         (ps:chain src-reference next-sibling)
                         dst-reference)))
            nil)
         `(let ((src-parent (js-node ,src-parent))
                (dst-parent (js-node ,dst-parent))
                (dst-reference (js-node ,reference)))
            (dotimes (_ ,length)
              (ps:chain dst-parent
                        (insert-before
                         (ps:chain src-parent first-child)
                         dst-reference)))
            nil))
     (host dst-parent))

    (iter (for node = (if src-reference (next-sibling src-reference)
                          (first-child src-parent)))
      (while node)
      (until (eql node end))
      (remove-node node)
      (insert-before dst-parent node reference)))
  (record-undo
   (nclo undo-move-nodes ()
     (move-nodes-2 dst-parent beg reference src-parent end))
   (nclo redo-move-nodes ()
     (move-nodes-2 src-parent beg end dst-parent reference))))

(defun move-nodes (beg end to)
  "Move nodes between BEG and END to TO and returns nil.

BEG and END must be sibling positions.  If END is nil, move children
starting from BEG till the end of its parent."
  (let* ((beg (resolve-marker beg))
         (end (resolve-marker end))
         (to (resolve-marker to))
         (src-parent (node-containing beg))
         (dst-parent (node-containing to))
         (host (host to)))
    ;; Account for this edge case
    (when (or (end-pos-p beg) (equalp beg end))
      (return-from move-nodes nil))
    (unless (host beg)
      (error "~a does not point inside an active document." beg))
    (unless (eq (host beg) host)
      (error "~a and ~a not point inside the same document." beg to))
    (check-read-only host)
    (setq end (maybe-split-text-node end)
          beg (maybe-split-text-node beg)
          to (maybe-split-text-node to))
    (move-nodes-2 src-parent beg end dst-parent to)
    (maybe-merge-text-nodes end)
    (maybe-merge-text-nodes to)
    (maybe-merge-text-nodes beg)
    nil))

;;; Additional operations

(defun splice-node (node)
  "Splice children of NODE in place of NODE itself."
  (move-nodes (pos-down node) nil (pos-right node))
  (delete-nodes node (pos-right node)))

(defun join-nodes (dst src)
  "Join DST and SRC nodes.

This moves all children of SRC into DST and deletes SRC."
  (move-nodes src (pos-right src) (pos-down-last dst))
  (splice-node src))

(defun raise-node (node)
  "Replace NODE's parent with NODE."
  (move-nodes node (pos-right node) (pos-up node))
  (delete-nodes (pos-right node) (pos-right (pos-right node))))

(defun split-node (&optional (pos (focus)))
  "Split node containing POS at POS.

Let parent be the node containing POS. This involves inserting a clone
of parent after parent, and moving children after POS into the clone."
  (let* ((node (node-containing pos))
         (new-node (clone-node node nil))
         (dst (pos-right (pos-up pos))))
    (insert-nodes dst new-node)
    (move-nodes pos nil (end-pos new-node))
    new-node))

(defun wrap-node (node new-node)
  "Insert NEW-NODE around NODE.

NODE become the last child of NEW-NODE."
  (insert-nodes node new-node)
  (move-nodes node (pos-right node) (end-pos new-node)))

(defun erase-buffer ()
  "Delete all content of current buffer."
  (delete-nodes (pos-down (document-root (current-buffer))) nil))

;;; Editing commands

(defun self-insert-char ()
  (let ((desc (key-description (lastcar *this-command-keys*))))
    (cond ((= (length desc) 1) (aref desc 0))
          ((equal desc "space") #\Space))))

(define-command self-insert-command ()
  (undo-auto-amalgamate)
  (insert-nodes (focus) (string (self-insert-char))))

(define-command new-line (&optional (marker (focus)))
  (insert-nodes marker (make-new-line-node)))

(defun trivial-p (node)
  (or (characterp node) (new-line-node-p node)))

(define-command backward-delete (&optional (marker (focus)))
  (undo-auto-amalgamate)
  (setf (adjust-marker-direction (host marker))
        'backward)
  (if-let (before (node-before marker))
    (if (trivial-p before)
        (delete-nodes (pos-left marker) marker)
        (backward-node marker))
    ;; We are at the beginning of NODE
    (let* ((node (node-containing marker))
           (prev (pos-left node)))
      (cond ((null (first-child node))
             (delete-nodes node (pos-right node)))
            ((trivial-p prev)
             (delete-nodes prev node))
            ((and (element-p prev)
                  (equal (attribute node "class")
                         (attribute prev "class")))
             (join-nodes prev node))
            (t (backward-node marker))))))

(define-command forward-delete (&optional (marker (focus)))
  (undo-auto-amalgamate)
  (if-let (after (node-after marker))
    (if (trivial-p after)
        (delete-nodes marker (pos-right marker))
        (forward-node marker))
    ;; We are at the beginning of NODE
    (let* ((node (node-containing marker))
           (next (pos-right node)))
      (cond ((null (first-child node))
             (delete-nodes node (pos-right node)))
            ((trivial-p next)
             (delete-nodes next (pos-right next)))
            ((and (element-p next)
                  (equal (attribute node "class")
                         (attribute next "class")))
             (join-nodes node next))
            (t (forward-node marker))))))

(define-command backward-delete-word (&optional (marker (focus)))
  (let ((end (pos marker)))
    (backward-word marker)
    (let ((start (pos marker)))
      (if (and (text-pos-p end)
               (eql (text-pos-node end)
                    (text-pos-node start)))
          (delete-nodes start end)
          (delete-nodes start nil)))))

(define-command backward-delete-element (&optional (marker (focus)))
  (backward-element marker)
  (delete-nodes marker (pos-right marker)))

(defvar *clipboard-ring* (containers:make-ring-buffer 1000 t))

(defvar *clipboard-ring-index* 0)

(define-command cut-element (&optional (pos (focus)))
  (setq pos (or (pos-up-ensure pos #'element-p)
                (error 'top-of-subtree)))
  (containers:insert-item *clipboard-ring* (extract-nodes pos 1)))

(define-command copy-element (&optional (pos (focus)))
  (setq pos (or (pos-up-ensure pos #'element-p)
                (error 'top-of-subtree)))
  (containers:insert-item *clipboard-ring* (list (clone-node pos))))

(define-command paste ()
  (let ((item (containers:item-at *clipboard-ring* *clipboard-ring-index*)))
    (setf (advance-p (selection-marker (current-buffer))) nil)
    (setf (pos (selection-marker (current-buffer))) (pos (focus)))
    (apply #'insert-nodes (focus) (mapcar #'clone-node item))))

(define-command paste-pop ()
  (incf *clipboard-ring-index*)
  (let ((item (containers:item-at *clipboard-ring* *clipboard-ring-index*)))
    (delete-nodes (selection-marker (current-buffer))
                  (pos-right (selection-marker (current-buffer))))
    (apply #'insert-nodes (focus) (mapcar #'clone-node item))))

(define-command forward-cut (&optional (pos (focus)))
  (iter (with end = (copy-pos pos))
    (setq end (npos-right end))
    (unless end
      (containers:insert-item *clipboard-ring* (extract-nodes pos nil))
      (return))
    (when (new-line-node-p end)
      (containers:insert-item *clipboard-ring* (extract-nodes pos end))
      (return))))

;;; Default key bindings

(define-keys global
  "backspace" 'backward-delete
  "space" 'self-insert-command
  "enter" 'new-line
  "M-backspace" 'backward-delete-word
  "C-M-backspace" 'backward-delete-element
  "C-d" 'forward-delete
  "M-d" 'forward-delete-word
  "C-w" 'cut-element
  "M-w" 'copy-element
  "C-y" 'paste
  "M-y" 'paste-pop
  "C-k" 'forward-cut)

(iter (for i from 32 below 127)
  (for char = (code-char i))
  (unless (member char '(#\ ))
    (define-key *global-keymap* (string char) 'self-insert-command)))
