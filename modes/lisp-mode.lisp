(in-package #:neomacs)

#+nil (define-command-global neomacs-scratch ()
  (let ((buffer (make-buffer :url (quri:uri "neomacs:scratch.lisp"))))
    (set-current-buffer buffer)))

(define-class lisp-mode (prog-mode) ()
  (:documentation "Lisp mode."))

(define-keymap lisp-mode ()
  'self-insert-command 'lisp-self-insert
  "tab" 'show-completions
  "M-(" 'wrap-paren
  "M-;" 'wrap-comment
  "M-r" 'lisp-raise
  "M-s" 'lisp-splice

  "C-M-x" 'eval-defun
  ;; "C-c C-c" 'compile-defun
  "tab" 'show-completions
  "C-x C-e" 'eval-last-expression
  "C-c C-p" 'eval-print-last-expression)

(defmethod selectable-p-aux ((buffer lisp-mode) pos)
  (if-let (node (node-after pos))
    (and (not (symbol-node-p node))
         (not (and (new-line-node-p node)
                   (non-empty-symbol-node-p (node-before pos)))))
    (not (non-empty-symbol-node-p (node-before pos)))))

(defmethod render-focus-aux ((buffer lisp-mode) pos)
  (match pos
    ((end-pos node)
     (if (non-empty-symbol-node-p node)
         (let ((next (next-sibling node)))
           (cond ((not next)
                  (call-next-method buffer (end-pos (parent node))))
                 (t (call-next-method))))
         (call-next-method)))
    (_ (call-next-method))))

(defmethod block-element-p-aux ((buffer lisp-mode) element)
  (if (equal (tag-name element) "div")
      (not (class-p element "list" "comment"))
      (call-next-method)))

(defmethod on-focus-move progn ((buffer lisp-mode) old new)
  (declare (ignore old))
  (let ((node (node-containing new)))
    (if (class-p node "list" "symbol" "doc")
        (enable 'sexp-editing-mode)
        (disable 'sexp-editing-mode))))

(defgeneric print-dom (object &key &allow-other-keys))

(defun compute-operator (node)
  (when (eql (first-child (parent node)) node)
    ""))

(defun compute-symbol (node)
  (ignore-errors
   (swank::parse-symbol (text (first-child node)))))

(defun compute-symbol-type (node)
  (when-let (symbol (compute-symbol node))
    (cond ((or (and (special-operator-p symbol)
                    (not (member symbol '(function))))
               (member symbol '(declare)))
           "special-operator")
          ((macro-function symbol) "macro")
          ((keywordp symbol) "keyword")
          (t ""))))

(defmethod on-node-setup progn ((buffer lisp-mode) node)
  (set-attribute-function node "operator" 'compute-operator)
  (add-observer (slot-value node 'parent)
                (lambda (cell)
                  (let ((parent (cell-ref cell)))
                    (when (symbol-node-p parent)
                      (let ((prev (previous-sibling node))
                            (next (next-sibling node)))
                        (cond ((and (not prev) (not next))
                               (raise-node node))
                              ((not prev)
                               (move-nodes node (pos-right node)
                                           parent))
                              ((not next)
                               (move-nodes node (pos-right node)
                                           (pos-right parent)))
                              (t (let ((second-symbol (split-node node)))
                                   (move-nodes node (pos-right node)
                                               second-symbol)))))))))
  (when (symbol-node-p node)
    (set-attribute-function node "symbol-type" 'compute-symbol-type)))

(defun make-list-node (children)
  (lret ((node (make-instance 'element :tag-name "div")))
    (setf (attribute node "class") "list")
    (iter (for c in children)
      (append-child node c))))

(defun make-atom-node (class text)
  (lret ((node (make-instance 'element :tag-name "span")))
    (setf (attribute node "class") class)
    (when (plusp (length text))
      (append-child node (make-instance 'text-node :text text)))))

(defun list-node-p (node)
  (class-p node "list"))

(defun atom-node-p (node)
  (class-p node "symbol" "string" "object" "comment"))

(defun sexp-node-p (node)
  (class-p node "list" "symbol" "string" "object"))

(defun symbol-node-p (node)
  (class-p node "symbol"))

(defun non-empty-symbol-node-p (node)
  (and (symbol-node-p node) (first-child node)))

(defun atom-node-text (node)
  (ignore-errors (text (first-child node))))

(defmethod print-dom ((cons cons) &key)
  (make-list-node (mapcar #'print-dom cons)))

(defmethod print-dom ((null null) &key)
  (make-list-node nil))

(defmethod print-dom ((symbol (eql '%br)) &key)
  (make-new-line-node))

(defmethod print-dom ((symbol symbol) &key)
  (make-atom-node "symbol"
                  (let ((*print-case* :downcase))
                    (prin1-to-string symbol))))

(defmethod print-dom ((string string) &key)
  (make-atom-node "string" string))

(defmethod print-dom ((obj t) &key)
  (make-atom-node "symbol" (prin1-to-string obj)))

(defun lisp-self-insert ()
  (undo-auto-amalgamate)
  (let ((string (string (self-insert-char)))
        (marker (focus)))
    (cond ((atom-node-p (node-containing marker))
           (insert-nodes marker string))
          ((class-p (node-before marker) "comment")
           (insert-nodes (end-pos (node-before marker)) string))
          (t
           (let ((node (make-atom-node "symbol" string)))
             (insert-nodes marker node)
             (setf (pos marker) (end-pos node)))))))

(define-command open-paren (&optional (marker (focus)))
  (let ((node (make-list-node nil)))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command wrap-paren (&optional (pos (focus)))
  (let ((node (make-list-node nil)))
    (setq pos (or (pos-up-ensure pos #'sexp-node-p)
                  (error 'top-of-subtree)))
    (wrap-node pos node)))

(define-command open-string (&optional (marker (focus)))
  (let ((node (make-atom-node "string" "")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command open-space (&optional (marker (focus)))
  (if (class-p (node-containing marker) "symbol")
      (setf (pos marker) (pos-down (split-node marker)))
      (let ((node (make-atom-node "symbol" "")))
        (insert-nodes marker node)
        (setf (pos marker) (end-pos node)))))

(define-command open-comment (&optional (marker (focus)))
  (labels ((cycle-level (n)
             (lret ((n (1+ (mod n 4))))
               (message "Comment Level -> ~a" n))))
    (if-let (node (comment-around marker))
      (setf (attribute node "comment-level")
            (prin1-to-string
             (cycle-level
              (parse-number:parse-number
               (attribute node "comment-level")))))
      (let ((node (make-atom-node "comment" "")))
        (setf (attribute node "comment-level") "1")
        (insert-nodes marker node)
        (setf (pos marker) (end-pos node))))))

(define-command wrap-comment (&optional (pos (focus)))
  (let ((node (make-atom-node "comment" "")))
    (setq pos (or (pos-up-ensure pos #'sexp-node-p)
                  (error 'top-of-subtree)))
    (wrap-node pos node)))

(define-command lisp-raise (&optional (pos (focus)))
  (setq pos (or (pos-up-ensure pos #'sexp-node-p)
                (error 'top-of-subtree)))
  (raise-node pos))

(define-command lisp-splice (&optional (pos (focus)))
  (setq pos (or (pos-up-until pos #'list-node-p)
                (error 'top-of-subtree)))
  (splice-node pos))

(define-class sexp-editing-mode () ()
  (:documentation "Editing S-exp."))

(define-keymap sexp-editing-mode ()
  "(" 'open-paren
  "\"" 'open-string
  "space" 'open-space
  ";" 'open-comment)

(defun parse-prefix (string)
  "Parse prefix from STRING.
Return a list of wrapper functions and the rest of STRING.  The list
of wrapper functions can be applied to some Lisp object one-by-one and
reproduce the effect of parsed prefix."
  (let (wrappers (i 0))
    (iter (while (< i (length string)))
      (case (aref string i)
        ((#\')
         (incf i)
         (push (lambda (next) (list 'quote next)) wrappers))
        ((#\`)
         (incf i)
         (push (lambda (next) (list 'sb-int:quasiquote next)) wrappers))
        ((#\,)
         (incf i)
         (case (when (< i (length string))
                 (aref string i))
           ((#\@)
            (incf i)
            (push (lambda (next) (sb-int:unquote next 2)) wrappers))
           (t
            (push (lambda (next) (sb-int:unquote next 0)) wrappers))))
        ((#\#)
         (incf i)
         (case (when (< i (length string))
                 (aref string i))
           ((#\')
            (incf i)
            (push (lambda (next) (list 'function next)) wrappers))
           (t (decf i) (return))))
        (t (return))))
    (values wrappers (subseq string i))))

(defun node-to-sexp (node)
  "Parse DOM NODE as a Lisp object.
It also takes into account any prefix preceding NODE."
  (labels ((ghost-symbol-p (node)
             (when (symbol-node-p node)
               (bind (((:values wrappers rest)
                       (parse-prefix (atom-node-text node))))
                 (when (zerop (length rest))
                   wrappers))))
           (apply-wrappers (wrappers node)
             (iter (for w in wrappers)
               (setq node (funcall w node)))
             node)
           (process (node)
             (let ((sexp
                     (cond ((list-node-p node)
                            (mapcar #'process
                                    (remove-if (alex:disjoin
                                                #'text-node-p
                                                #'ghost-symbol-p
                                                #'new-line-node-p)
                                               (child-nodes node))))
                           ((equal "string" (attribute node "class"))
                            (atom-node-text node))
                           ((symbol-node-p node)
                            (bind (((:values wrappers rest)
                                    (parse-prefix (atom-node-text node))))
                              (apply-wrappers wrappers (read-from-string rest))))
                           ((new-line-node-p node) nil)
                           (t (error "Unrecognized DOM node: ~a" node)))))
               (if-let (wrappers (ghost-symbol-p (previous-sibling node)))
                 (apply-wrappers wrappers sexp)
                 sexp))))
    (process node)))

(defun current-package (&optional (marker-or-pos (focus)))
  "Return the package in the context of MARKER-OR-POS.
This is determined by searching for a `in-package' top-level form
before MARKER-OR-POS."
  (with-marker (marker marker-or-pos)
    (or (find-package
         (handler-case
             (iter (beginning-of-defun marker)
               (for node = (node-after marker))
               (when (and (list-node-p node)
                          (eql 'in-package (compute-symbol (first-child node))))
                 (let ((p (npos-right-until (first-child node) #'sexp-node-p)))
                   (when-let (s (compute-symbol p))
                     (return (symbol-name s))))))
           (top-of-subtree ())))
        (find-package "NEOMACS"))))

(define-command eval-defun (&optional (marker-or-pos (focus)))
  (with-marker (marker marker-or-pos)
    (beginning-of-defun marker)
    (let* ((*package* (current-package marker))
           (result (eval (node-to-sexp (pos marker)))))
      (message "=> ~a" result))))

(defun last-expression (pos)
  (iter
    (unless pos (error 'beginning-of-subtree))
    (for node = (node-before pos))
    (until (sexp-node-p node))
    (setq pos (npos-prev pos)))
  (node-before pos))

(define-command eval-last-expression (&optional (marker (focus)))
  (let* ((*package* (current-package marker))
         (result (eval (node-to-sexp (last-expression (pos marker))))))
    (message "=> ~a" result)))

(define-command eval-print-last-expression (&optional (marker (focus)))
  (let* ((*package* (current-package marker))
         (node (last-expression marker))
         (pos (pos-right node))
         (last-line (make-new-line-node)))
    (if (new-line-node-p (node-after pos))
        (setf pos (pos-right pos))
        (insert-nodes pos (make-new-line-node)))
    (insert-nodes pos (print-dom (eval (node-to-sexp node)))
                  last-line)
    (setf (pos marker) (pos-right last-line))))

(defun atom-around (pos)
  (let ((parent (node-containing pos))
        (before (node-before pos))
        (after (node-after pos)))
    (cond ((atom-node-p parent) parent)
          ((atom-node-p before) before)
          ((atom-node-p after) after))))

(defun symbol-around (pos)
  (when-let (node (atom-around pos))
    (when (equal "symbol" (attribute node "class"))
      node)))

(defun comment-around (pos)
  (when-let (node (atom-around pos))
    (when (equal "comment" (attribute node "class"))
      node)))

(defmethod compute-completion ((buffer lisp-mode) pos)
  (when-let (node (symbol-around pos))
    (let* ((package (current-package pos))
           (swank::*buffer-package* package)
           (completions (car (swank:fuzzy-completions
                              (atom-node-text node)
                              package))))
      (list (range (pos-down node) (pos-down-last node))
            completions))))

;;; Parser

(defun read-string (stream c)
  (declare (ignore c))
  (append-child
   *dom-output*
   (make-atom-node
    "string"
    (iter (for c = (read-char stream))
      (until (eql c #\"))
      (if (eql c #\\)
          (collect (read-char stream) result-type string)
          (collect c result-type string))))))

(defun read-line-comment (stream c)
  (declare (ignore c))
  (let ((n 1))
    (iter (for c = (peek-char t stream nil nil t))
      (while (eql c #\;))
      (incf n)
      (read-char stream))
    (bind (((:values line eof-p) (read-line stream))
           (node (make-atom-node "comment" line)))
      (setf (attribute node "comment-level") (prin1-to-string n))
      (append-child *dom-output* node)
      (unless eof-p (unread-char #\Newline stream)))))

(defvar *lisp-syntax-table*
  (lret ((table (make-syntax-table)))
    (set-syntax-range table 33 127 'symbol)
    (setf (get-syntax-table #\( table) (make-read-delimited #\)))
    (setf (get-syntax-table #\) table) nil)
    (setf (get-syntax-table #\  table) 'read-ignore)
    (setf (get-syntax-table #\Newline table) 'read-newline)
    (setf (get-syntax-table #\Tab table) 'read-ignore)
    (setf (get-syntax-table #\" table) 'read-string)
    (setf (get-syntax-table #\\ table) 'single-escape)
    (setf (get-syntax-table #\; table) 'read-line-comment)))

(defmethod read-dom-aux ((buffer lisp-mode) stream)
  (let ((*syntax-table* *lisp-syntax-table*))
    (read-dispatch *syntax-table* stream)))

;;; Pretty printer

(defun symbol-indentation (symbol)
  (case symbol
    ((lambda) 1)
    ((block catch return-from throw eval-when
            multiple-value-call multiple-value-prog1
            unwind-protect)
     1)
    ((locally progn) 0)
    ((progv) 1)
    ((flet labels macrolet)
     '((&whole 4 &rest (&whole 1 4 4 &rest 2)) &rest 2))
    ((let let* symbol-macrolet dx-let)
     '((&whole 4 &rest (&whole 1 1 2)) &rest 2))
    ((case ccase ecase)
     '(4 &rest (&whole 2 &rest 1)))
    ((handler-case handler-bind) 1)
    (t (when-let (mf (macro-function symbol))
         (or (sb-pretty::macro-indentation mf)
             (when (sera:string-prefix-p "DEF" (symbol-name symbol))
               1))))))

(defun normalize-indent-spec (indent-spec)
  (when indent-spec
    (when (numberp indent-spec)
      (setq indent-spec
            (nconc (make-list indent-spec :initial-element 4)
                   (list '&rest 2))))
    (push nil indent-spec)
    indent-spec))

(defun pprint-form (list-node stream indent-spec)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (if indent-spec
        (iter
          (with prev = nil)
          (for c in (child-nodes list-node))
          (unless (or (first-iteration-p)
                      (new-line-node-p prev))
            (write-char #\space stream))
          (if (sexp-node-p c)
              (let* ((this-indent-spec
                       (if (eql (car indent-spec) '&rest)
                           (cadr indent-spec)
                           (pop indent-spec)))
                     (next-indent-spec
                       (if (eql (car indent-spec) '&rest)
                           (cadr indent-spec)
                           (car indent-spec)))
                     (indent-number
                       (or (when (numberp next-indent-spec)
                             next-indent-spec)
                           (getf next-indent-spec '&whole)
                           1)))
                (pprint-indent :block (1- indent-number) stream)
                (if (and (listp this-indent-spec) (list-node-p c))
                    (progn
                      (setq this-indent-spec (copy-tree this-indent-spec))
                      (remf this-indent-spec '&whole)
                      (pprint-form c stream this-indent-spec))
                    (write c :stream stream)))
              (write c :stream stream))
          (setq prev c))
        ;; Print function call form
        (iter
          (with prev = nil)
          (with i = 0)
          (for c in (child-nodes list-node))
          (unless (or (first-iteration-p)
                      (new-line-node-p prev))
            (write-char #\space stream))
          (when (and (sexp-node-p c) (<= i 1))
            (pprint-indent :current 0 stream))
          (write c :stream stream)
          (when (sexp-node-p c)
            (incf i))
          (setq prev c)))))

(defvar *lisp-pprint-dispatch*
  (lret ((*print-pprint-dispatch*
          (copy-pprint-dispatch *print-pprint-dispatch*)))
    (set-pprint-dispatch
     'element
     (lambda (stream self &rest noise)
       (declare (ignore noise))
       (cond ((list-node-p self)
              (pprint-form self stream
                           (normalize-indent-spec
                            (symbol-indentation
                             (compute-symbol (first-child self))))))
             ((new-line-node-p self)
              (pprint-newline :mandatory stream))
             ((symbol-node-p self)
              (write-string (atom-node-text self) stream))
             ((equal (attribute self "class") "string")
              (write
               (with-output-to-string (s)
                 (iter (for c in (child-nodes self))
                   (write c :stream s)))
               :stream stream))
             ((equal (attribute self "class") "comment")
              (dotimes (_ (parse-number:parse-number (attribute self "comment-level")))
                (write-char #\; stream))
              (write-char #\  stream)
              (iter (for c in (child-nodes self))
                (write c :stream stream)))
             (t (error "TODO")))))

    (set-pprint-dispatch
     'text-node
     (lambda (stream self &rest noise)
       (declare (ignore noise))
       (write-string (text self) stream)))))

(defmethod write-dom-aux ((buffer lisp-mode) node stream)
  (let ((*print-pprint-dispatch* *lisp-pprint-dispatch*)
        (*package* (find-package "NEOMACS"))
        (*print-pretty* t))
    (prin1 node stream)))

;;; Style

(defstyle sexp-node `(((:append ":not(:last-child):not(.focus-tail)")
                       :margin-right "0.4em")
                      :display "inline-block"
                      :vertical-align "top"
                      :position "relative"
                      :white-space "pre-wrap"))
(defstyle list-node
    `(((:append "::before")
       :content "("
       :margin-left "-0.4em")
      ((:append "::after")
       :content ")"
       :vertical-align "bottom")
      ((:append ":not(:last-child)")
       :margin-right "0.4em")
      ((:append ".focus-tail::after")
       :content ")"
       :width "auto"
       :inherit selection)
      ((:append ".focus::before")
       :inherit selection)
      :inherit sexp-node
      :padding-left "0.4em"))

(defstyle string-node `(((:append "::before") :content "\"")
                        ((:append "::after") :content "\"")
                        ((:append ":not(:last-child)")
                         :margin-right "0.4em")
                        ((:append ".focus::before")
                         :inherit selection)
                        ((:append ".focus-tail::after")
                         :content "\""
                         :width "auto"
                         :inherit selection)
                        :inherit (sexp-node string)))
(defstyle symbol-node `(:inherit sexp-node))
(defstyle empty-symbol-node `(((:append "::after")
                               :content "_")
                              ((:append ":not(:last-child)")
                               :margin-right "0.4em")
                              ((:append ".focus-tail::after")
                               :content "_"
                               :width "auto"
                               :inherit selection)))
(defstyle comment-node `(((:append "::after") :content "⁣")
                         :display "inline-block"
                         :white-space "pre-wrap"))
(defstyle comment-node-1 `(:position "sticky"
                           :left "20em"
                           :border-left "0.3rem solid #a997a0"
                           :padding-left "0.3rem"
                           :inherit comment))
(defstyle comment-node-2 `(:border-left "0.3rem solid #a997a0"
                           :padding-left "0.3rem"
                           :inherit comment))
(defstyle comment-node-3 `(((:append "::before")
                            :content ""
                            :display "list-item"
                            :position "absolute")
                           :font-size "1.2em"
                           :inherit comment))
(defstyle comment-node-4 `(((:append "::before")
                            :content ""
                            :display "list-item"
                            :position "absolute")
                           :font-size "1.1em"
                           :inherit comment))

(defstyle lisp-mode
    `((".symbol" :inherit symbol-node)
      (".symbol:empty" :inherit empty-symbol-node)
      (".string" :inherit string-node)
      (".object" :inherit symbol-node)
      (".symbol[symbol-type=\"macro\"][operator]" :inherit macro)
      (".symbol[symbol-type=\"keyword\"]" :inherit keyword)
      (".symbol[symbol-type=\"special-operator\"][operator]" :inherit special-operator)
      (".list" :inherit list-node)
      (".comment" :inherit comment-node)
      (".comment[comment-level=\"1\"]" :inherit comment-node-1)
      (".comment[comment-level=\"2\"]" :inherit comment-node-2)
      (".comment[comment-level=\"3\"]" :inherit comment-node-3)
      (".comment[comment-level=\"4\"]" :inherit comment-node-4)))

;;; Default hooks
#+nil (add-mode-hook 'lisp-mode 'undo-mode)
#+nil (add-mode-hook 'lisp-mode 'auto-completion-mode)
