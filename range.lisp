(in-package #:neomacs)

(sera:export-always
    '(range range-p range-collapsed-p
      before-p inside-range-p inside-range-inclusive-p
      extract-range delete-range
      set-selection))

(defun ancestors (pos)
  "Return list of ancestor position of POS.
POS itself appears at the beginning, Root appears at the end."
  (iter (while pos)
    (collect pos)
    (setq pos (node-containing pos))))

(defun remove-common-prefix (list-1 list-2)
  (iter
    (unless (eql (car list-1) (car list-2))
      (return (values list-1 list-2)))
    (pop list-1)
    (pop list-2)
    (while (or list-1 list-2))))

(defun before-p (pos-1 pos-2)
  "Test if POS-1 is strictly before POS-2 in preorder traversal."
  (unless (equalp pos-1 pos-2)
    (bind ((a-1 (nreverse (ancestors pos-1)))
           (a-2 (nreverse (ancestors pos-2)))
           ((:values tail-1 tail-2)
            (remove-common-prefix a-1 a-2))
           (n-1 (car tail-1))
           (n-2 (car tail-2)))
      (unless n-1 (return-from before-p t))
      (unless n-2 (return-from before-p nil))
      (when (end-pos-p n-1) (return-from before-p nil))
      (when (end-pos-p n-2) (return-from before-p t))
      (when (and (text-pos-p n-1)
                 (text-pos-p n-2)
                 (eql (text-pos-node n-1) (text-pos-node n-2)))
        (return-from before-p (< (text-pos-offset n-1)
                                 (text-pos-offset n-2))))
      (when (text-pos-p n-1) (setq n-1 (text-pos-node n-1)))
      (when (text-pos-p n-2) (setq n-2 (text-pos-node n-2)))
      (iter (for c first (first-child (parent n-1))
                 then (next-sibling c))
        (when (eql c n-1)
          (return-from before-p t))
        (when (eql c n-2)
          (return-from before-p nil))
        (unless c
          (error "Should not reach here!"))))))

(defstruct (range (:constructor %range (beg end)))
  "Denotes a range before BEG (inclusive) and END (exclusive)."
  (beg (error "Must supply BEG.") :type pos)
  (end (error "Must supply END.") :type pos))

(defun range (beg end)
  "Create a range between BEG and END.

This normalizes `range-beg' and `range-end', i.e. if END is before
BEG, they are swapped."
  (if (before-p end beg)
      (%range end beg)
      (%range beg end)))

(defun range-collapsed-p (range)
  "Test if RANGE is collapsed (`range-beg' and `range-end' are the same)."
  (equalp (range-beg range) (range-end range)))

(defun copy-nodes (start end)
  "Copy nodes between START and END, which must be sibling positions.

Like `extract-ndoes', but only clone nodes, don't actually delete nodes.
END might also be nil, which designates the end of node containing START."
  (unless (or (not end)
              (eq (node-containing start)
                  (node-containing end)))
    (warn "~a and ~a are not siblings" start end))
  (let (nodes)
    (block nil
      (match start
        ((text-pos node offset)
         ;; Selecting inside a text node
         (when (and (text-pos-p end)
                    (eq (text-pos-node end) node))
           (push
            (make-instance
             'text-node :text
             (subseq (text node) offset (text-pos-offset end)))
            nodes)
           (return))

         (push (make-instance
                'text-node :text
                (subseq (text node) offset))
               nodes)
         (if-let (next (next-sibling node))
           (setq start next)
           (return)))
        ((end-pos)
         (return)))
      (match end
        ((text-pos node offset)
         (when (> offset 0)
           (push (make-instance
                  'text-node :text
                  (subseq (text node) 0 offset))
                 nodes))
         (setq end node))
        ((end-pos) (setq end nil)))
      (iter (for node first start then (next-sibling node))
        (until (eql node end))
        (while node)
        (push (clone-node node) nodes)))
    (nreverse nodes)))

(defun map-range-collect (range fn)
  "Decompose RANGE into a set of sibling ranges and map FN over them.

Like `map-range', but also collect the result into a DOM tree."
  (unless (range-collapsed-p range)
    (bind ((a-1 (nreverse (ancestors (range-beg range))))
           (a-2 (nreverse (ancestors (range-end range))))
           ((:values tail-1 tail-2)
            (remove-common-prefix a-1 a-2)))
      (cond ((not tail-1)
             (let ((root (clone-node (parent (car tail-2)) nil)))
               (iter (with parent = root)
                 (for tail on tail-2)
                 (for pos = (car tail))
                 (append-children
                  parent
                  (funcall fn (pos-down (node-containing pos)) pos))
                 (when (cdr tail)
                   (setq parent (append-child parent (clone-node pos nil)))))
               (list root)))
            ((not tail-2)
             (error "Should not reach here!"))
            (t
             (let ((nodes
                     (funcall fn (if (cdr tail-1)
                                     (pos-right (car tail-1))
                                     (car tail-1))
                              (car tail-2))))
               (when (cdr tail-1)
                 (push (clone-node (car tail-1) nil) nodes))
               (iter (with parent = (car nodes))
                 (for tail on (cdr tail-1))
                 (for pos = (car tail))
                 (if (cdr tail)
                     (progn
                       (append-child parent (clone-node pos nil))
                       (append-children
                        parent
                        (funcall fn (pos-right pos) nil))
                       (setq parent (first-child parent)))
                     (append-children
                      parent
                      (funcall fn (car tail)
                               nil))))
               (when (cdr tail-2)
                 (alex:nconcf nodes (list (clone-node (car tail-2) nil)))
                 (iter (with parent = (lastcar nodes))
                   (for tail on (cdr tail-2))
                   (for pos = (car tail))
                   (append-children
                    parent
                    (funcall fn (pos-down (node-containing pos)) pos))
                   (when (cdr tail)
                     (setq parent (append-child parent (clone-node pos nil))))))
               nodes))))))

(defun extract-range (range)
  "Extract contents inside RANGE.
This may extract part of a node, examples (^ marks `range-beg' and
_ marks `range-end'):
DOM before => DOM after, returned nodes
((a^ b) c (d _e)) => ((a)(e)), ( b) c (d )
^(a (b _c) d) => ((c) d), (a (b ))"
  (map-range-collect range #'extract-nodes))

(defun clone-range (range)
  "Like `extract-range', but only copy and does not mutate the DOM."
  (map-range-collect range #'copy-nodes))

(defun map-range (range fn)
  "Decompose RANGE into a set of sibling ranges and call FN on them.

FN is called with two arguments BEG and END, which are sibling
positions. END might be nil, which designates the end of node
containing BEG."
  (unless (range-collapsed-p range)
    (bind ((a-1 (nreverse (ancestors (range-beg range))))
           (a-2 (nreverse (ancestors (range-end range))))
           ((:values tail-1 tail-2)
            (remove-common-prefix a-1 a-2)))
      (cond ((not tail-1)
             (iter (for pos in tail-2)
               (funcall fn (pos-down (node-containing pos)) pos)))
            ((not tail-2)
             (error "Should not reach here!"))
            (t
             (funcall fn (if (cdr tail-1)
                             (pos-right (car tail-1))
                             (car tail-1))
                      (car tail-2))
             (iter (for tail on (cdr tail-1))
               (funcall fn (if (cdr tail)
                               (pos-right (car tail))
                               (car tail))
                        nil))
             (iter (for pos in (cdr tail-2))
               (funcall fn (pos-down (node-containing pos)) pos))))
      nil)))

(defun delete-range (range)
  "Delete contents inside RANGE.
This may delete part of a node, examples (^ marks `range-beg' and
_ marks `range-end'):
DOM before => DOM after
((a^ b) c (d _e)) => ((a)(e))
^(a (b _c) d) => ((c) d)"
  (map-range range #'delete-nodes))

(defun inside-range-p (marker-or-pos range)
  "Test if MARKER-OR-POS is inside RANGE."
  (let ((pos (resolve-marker marker-or-pos))
        (beg (range-beg range))
        (end (range-end range)))
    (and (or (equalp beg pos) (before-p beg pos))
         (before-p pos end))))

(defun inside-range-inclusive-p (marker-or-pos range)
  "Test if MARKER-OR-POS is inside RANGE, including range-end."
  (let ((pos (resolve-marker marker-or-pos))
        (beg (range-beg range))
        (end (range-end range)))
    (and (or (equalp beg pos) (before-p beg pos))
         (or (equalp end pos) (before-p pos end)))))

(defun render-sibling-selection (start end)
  "Render selection between sibling positions START and END.

END may also be nil, which means render selection till end of
containing node."
  (setq start (resolve-marker start)
        end (resolve-marker end))
  (unless (or (not end)
              (eq (node-containing start)
                  (node-containing end)))
    (warn "~a and ~a are not siblings" start end))
  (match start
    ((text-pos node offset)
     ;; Selecting inside a text node
     (when (and (text-pos-p end)
                (eq (text-pos-node end) node))
       (render-text-focus node offset (text-pos-offset end)
                          "neomacs-range")
       (return-from render-sibling-selection))

     (render-text-focus node offset (length (text node))
                        "neomacs-range")
     (if-let (next (next-sibling node))
       (setq start next)
       (return-from render-sibling-selection)))
    ((end-pos)
     (return-from render-sibling-selection)))
  (match end
    ((text-pos node offset)
     (render-text-focus node 0 offset
                        "neomacs-range")
     (setq end node))
    ((end-pos) (setq end nil)))
  (iter (for node first start then (next-sibling node))
    (until (eql node end))
    (while node)
    (cond
      ((new-line-node-p node)) ;; TODO
      ((element-p node) (render-element-focus node "range-selection"))
      ((text-node-p node)
       (render-text-focus node 0 (length (text node))
                          "neomacs-range")))))

(defun render-range-selection (range)
  "Render RANGE selection."
  (map-range range #'render-sibling-selection))

(defun clear-range-selection (buffer)
  "Clear any rendered selection in BUFFER from renderer."
  (evaluate-javascript
   (ps:ps
     (clear-class "range-selection")
     (let ((highlight (ps:chain -c-s-s highlights (get "neomacs-range"))))
       (when highlight (ps:chain highlight (clear)))))
   buffer))

(define-command set-selection ()
  (setf (selection-active (current-buffer)) t
        (pos (selection-marker (current-buffer)))
        (pos (focus))))

(define-keys global
  "C-space" 'set-selection)
