(in-package #:neomacs)

(define-mode html-doc-mode (lisp-mode file-mode) ())

(define-keys html-doc-mode
  "enter" 'insert-paragraph
  "space" 'insert-no-break-space-maybe
  "C-c h" 'increase-heading
  "C-c C-h" 'decrease-heading
  "C-c c" 'insert-code
  "C-c b" 'insert-bold
  "C-c i" 'insert-italic
  "C-c u" 'insert-underline
  "C-c t" 'insert-description-list
  "C-c d" 'insert-description
  "C-c C-u" 'insert-unordered-list
  "C-c o" 'insert-ordered-list
  "C-c p" 'insert-code-block
  "C-c ," 'insert-comma
  "C-c C-l" 'insert-link
  "C-c C-o" 'open-link)

(defmethod sexp-parent-p ((buffer html-doc-mode) node)
  (class-p node "list"))

(defmethod revert-buffer-aux :around ((buffer html-doc-mode))
  (erase-buffer)
  (when (uiop:file-exists-p (file-path buffer))
    (load-url buffer (file-path-url (file-path buffer))))
  ;; Enter recursive edit to wait for buffer to load, so that
  ;; buffer state is updated when `revert-buffer' returns.
  (recursive-edit
   (lambda () (eql (load-status buffer) :loading))
   nil))

;; Avoid setting focus-tail to body when loading from file URL
(defmethod render-focus-aux ((buffer html-doc-mode) pos)
  (unless (eql (load-status buffer) :loading)
    (call-next-method)))

(defmethod on-buffer-loaded progn ((buffer html-doc-mode) url err)
  (when (equal url (file-path-url (file-path buffer)))
    (unless err
      (update-document-model buffer)
      (mapc (alex:curry #'do-dom (alex:curry #'on-node-setup buffer))
            (child-nodes (document-root buffer)))
      (setf (pos (focus buffer))
            (pos-down (document-root buffer))))))

(defun white-space-p (node)
  (member node '(#\Space #\Newline #\Tab)))

(defmethod selectable-p-aux ((buffer html-doc-mode) pos)
  (let ((after (node-after pos)))
    (and (not (tag-name-p after "p"))
         (if (characterp after)
             (and (find-if-not #'white-space-p (text (text-pos-node pos)))
                  (not (and (white-space-p after)
                            (white-space-p (node-before pos)))))
             t)
         (call-next-method))))

(defmethod trivial-p-aux ((buffer html-doc-mode) (node text-node))
  (every #'white-space-p (text node)))

(defun heading-text-to-id (text)
  (str:replace-all " " "-" (string-downcase text)))

(defun compute-heading-id (node)
  (heading-text-to-id (text-content node)))

(defmethod on-node-setup progn ((buffer html-doc-mode) (node element))
  (when (class-p node "comma-expr")
    (setf (attribute node 'keymap) *sexp-node-keymap*))
  (when (ppcre:all-matches "^h[123456]$" (tag-name node))
    (set-attribute-function node "id" #'compute-heading-id))
  (when (tag-name-p node "pre")
    (setf (attribute node 'keymap) *plaintext-node-keymap*)))

(defmethod on-node-setup progn ((buffer html-doc-mode) (node text-node))
  (with-post-command (node 'parent 'text)
    (let ((parent (parent node)))
      (unless (sexp-node-p parent)
        (when (and (allow-block-element-p parent)
                   (find-if-not #'white-space-p (text node)))
          (let ((new-node (make-element "p")))
            (insert-nodes (text-pos node 0) new-node)
            (move-nodes (text-pos node 0) (next-sibling node)
                        (end-pos new-node))))))))

(defun allow-block-element-p (parent)
  (member (tag-name parent)
          '("div" "li" "article" "section" "main" "aside" "header" "footer" "nav" "body")
          :test 'equal))

(defun check-valid-parent (parent child-tag)
  (unless (allow-block-element-p parent)
    (user-error "~a element not allowed in ~a" child-tag parent)))

(define-command insert-paragraph
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((pos (resolve-marker marker)))
    (if (allow-block-element-p (node-containing pos))
        (let ((new-node (make-element "p")))
          (insert-nodes pos new-node)
          (setf (pos marker) (pos-down new-node)))
        (split-node pos))))

(define-command insert-no-break-space-maybe
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((*this-command* 'self-insert-command))
    (undo-auto-amalgamate))
  (if (white-space-p (node-before marker))
      (insert-nodes marker (string #\No-break_space))
      (insert-nodes marker " "))
  (setf (adjust-marker-direction (current-buffer)) 'backward))

(defun cycle-heading (marker delta)
  (labels ((cycle-level (n)
             (lret ((n (mod (+ n delta) 7)))
               (message "Heading Level -> ~a" n)))
           (tag-level (tag)
             (cond ((equal tag "p") 0)
                   ((ppcre:all-matches "^h[123456]$" tag)
                    (parse-integer (subseq tag 1)))))
           (level-tag (level)
             (if (= level 0) "p"
                 (format nil "h~a" level))))
    (let* ((node (node-containing marker))
           (new-node (make-element
                      (level-tag
                       (cycle-level
                        (tag-level (tag-name node)))))))
      (insert-nodes (pos-right node) new-node)
      (move-nodes (pos-down node) nil (end-pos new-node))
      (delete-node node))))

(define-command increase-heading
  :mode html-doc-mode (&optional (marker (focus)))
  (cycle-heading marker 1))

(define-command decrease-heading
  :mode html-doc-mode (&optional (marker (focus)))
  (cycle-heading marker -1))

(define-command insert-code
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((node (make-element "code")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command insert-italic
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((node (make-element "i")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command insert-bold
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((node (make-element "b")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command insert-underline
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((node (make-element "u")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(defun insert-list (marker list-tag item-tag)
  (let ((parent (node-containing marker)))
    (unless (tag-name-p parent list-tag)
      (when (tag-name-p parent "p")
        (setf (pos marker) (split-node (pos marker))))
      (check-valid-parent (node-containing marker) list-tag)
      (let ((node (make-element list-tag)))
        (insert-nodes marker node)
        (setf (pos marker) (end-pos node)))))
  (let ((node (make-element item-tag)))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command insert-unordered-list
  :mode html-doc-mode (&optional (marker (focus)))
  (insert-list marker "ul" "li"))

(define-command insert-ordered-list
  :mode html-doc-mode (&optional (marker (focus)))
  (insert-list marker "ol" "li"))

(define-command insert-description-list
  :mode html-doc-mode (&optional (marker (focus)))
  (insert-list marker "dl" "dt"))

(define-command insert-description
  :mode html-doc-mode (&optional (marker (focus)))
  (insert-list marker "dl" "dd"))

(define-command insert-comma
  :mode html-doc-mode (&optional (marker (focus)))
  "Insert a Sexp list and change the surrounding node to a comma expr."
  (let* ((list (make-list-node nil)))
    (let ((*record-attribute-undo* t)
          (node (node-containing marker)))
      (add-class node "comma-expr")
      (setf (attribute node 'keymap) *sexp-node-keymap*))
    (insert-nodes marker list)
    (setf (pos marker) (end-pos list))))

(define-command insert-link
  :mode html-doc-mode (&optional (marker (focus)))
  "Insert link, or edit link target under focus."
  (if-let (node (pos-up-ensure (pos marker)
                               (alex:rcurry #'tag-name-p "a")))
    (let* ((href (read-from-minibuffer "Edit Href: " :initial
                                       (attribute node "href"))))
      (setf (attribute node "href") href))
    (let* ((href (read-from-minibuffer "Href: "))
           (a (make-element "a" :href href)))
      (insert-nodes marker a)
      (setf (pos marker) (end-pos a)))))

(define-command insert-code-block
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((parent (node-containing marker)))
    (when (tag-name-p parent "p")
      (setf (pos marker) (split-node (pos marker))))
    (check-valid-parent (node-containing marker) "pre")
    (let ((node (make-element "pre")))
      (insert-nodes marker node)
      (setf (pos marker) (end-pos node)))))

(define-command open-link
  :mode html-doc-mode (&optional (marker (focus)))
  "Open link under focus."
  (if-let (node (pos-up-ensure (pos marker)
                               (alex:rcurry #'tag-name-p "a")))
    (progn
      (push-global-marker)
      (if-let (web-url (look-like-url-p (attribute node "href")))
        (find-url web-url)
        (let ((url (quri:uri (attribute node "href"))))
          (with-current-buffer
              (find-file (merge-pathnames (quri:uri-path url)
                                          (file-path (current-buffer))))
            (when-let
                (node (block nil
                        (do-elements
                            (lambda (child)
                              (when (equal (attribute child "id") (quri:uri-fragment url))
                                (return child)))
                          (document-root (current-buffer)))))
              (setf (pos (focus)) node))))))
    (user-error "No link under focus")))

;;; Get DOM from renderer
;; Initially adapted from Nyxt
;; Copyright (c) 2017-2024, Atlas Engineer LLC.

(defparameter +get-body-json-code+
  (ps:ps
    (defparameter neomacs-identifier-counter 0)
    (defun process-element (element)
      (let ((object (ps:create :name (ps:@ element node-name)))
            (attributes (ps:chain element attributes)))
        (when (= 1 (ps:@ element node-type))
          (ps:chain element (set-attribute
                             "neomacs-identifier"
                             (ps:stringify neomacs-identifier-counter)))
          (incf neomacs-identifier-counter))
        (unless (ps:undefined attributes)
          (setf (ps:@ object :attributes) (ps:create))
          (loop for i from 0 below (ps:@ attributes length)
                do (setf (ps:@ object :attributes (ps:chain attributes (item i) name))
                         (ps:chain attributes (item i) value))))
        (unless (or (ps:undefined (ps:chain element child-nodes))
                    (= 0 (ps:chain element child-nodes length)))
          (setf (ps:chain object :children)
                (loop for child in (ps:chain element child-nodes)
                      collect (process-element child))))
        (when (and (ps:@ element shadow-root)
                   (ps:@ element shadow-root first-child))
          (setf (ps:chain object :children)
                (loop for child in (ps:chain *array
                                             (from (ps:@ element shadow-root children))
                                             (concat (ps:chain *array (from (ps:@ element children)))))
                      collect (process-element child))))
        (when (or (equal (ps:@ element node-name) "#text")
                  (equal (ps:@ element node-name) "#comment")
                  (equal (ps:@ element node-name) "#cdata-section"))
          (setf (ps:@ object :text) (ps:@ element text-content)))
        object))
    (list (process-element (ps:@ document body))
          neomacs-identifier-counter)))

(defun named-json-parse (json)
  "Return a DOM-tree produced from JSON.

JSON should have the format like what `+get-body-json-code+' produces:
- A nested hierarchy of objects (with only one root object), where
  - Every object has a 'name' (usually a tag name or '#text'/'#comment').
  - Some objects can have 'attributes' (a string->string dictionary).
  - Some objects have a subarray ('children') of objects working by these three
    rules."
  (labels ((json-to-dom (json)
             (let ((node
                     (cond
                       ((equal (assoc-value json :name) "#text")
                        (make-instance 'text-node :text (assoc-value json :text)))
                       (t
                        (make-instance 'element :tag-name (str:downcase (assoc-value json :name)))))))
               (dolist (c (assoc-value json :children))
                 (append-child node (json-to-dom c)))
               (iter (for (k . v) in (assoc-value json :attributes))
                 (if (eql k :neomacs-identifier)
                     (setf (id node) v)
                     (setf (attribute node (str:downcase (symbol-name k))) v)))
               node)))
    (json-to-dom json)))

(defun update-document-model (buffer)
  (bind (((json id) (evaluate-javascript-sync +get-body-json-code+ buffer))
         (dom (named-json-parse json)))
    (do-dom (lambda (n) (setf (host n) buffer)) dom)
    (setf (document-root buffer) dom
          (pos (focus buffer)) (pos-down dom)
          (next-neomacs-id buffer) id)))

(defmethod write-dom-aux ((buffer html-doc-mode) node stream)
  ;; `clone-node' to remove `neomacs-identifier'
  (serialize (clone-node node) stream))

(defmethod save-buffer-aux ((buffer html-doc-mode))
  (with-open-file (s (file-path buffer)
                     :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (serialize-document
       ;; `clone-node' to remove `neomacs-identifier'
       (clone-node (document-root buffer))
       nil s)
      nil)))

(defun print-arglist (arglist package)
  (let ((*package* package))
    (if arglist (format nil "~a" arglist) "()")))

(defun render-doc-string-paragraph (p)
  (let ((last-end 0))
    (append
     (iter
       (for (start end) on
            (ppcre:all-matches "`[^']*'" p)
            by #'cddr)
       (when (> start last-end)
         (collect (subseq p last-end start)))
       (when (> (1- end) (1+ start))
         (collect (make-element "code" :children
                                (list (subseq p (1+ start) (1- end))))))
       (setq last-end end))
     (when (> (length p) last-end)
       (list (subseq p last-end))))))

(defun render-doc-string (string)
  (when string
    (let ((paragraphs (str:split "

"
                                 string)))
      (iter (for p in paragraphs)
        (append-child
         *dom-output*
         (make-element
          "dd" :children
          (render-doc-string-paragraph p)))))))

(defun function-short-description (function)
  (cond ((macro-function function)
         (append-text *dom-output* "Macro"))
        ((typep (symbol-function function)
                'generic-function)
         (let ((*print-case* :capitalize))
           (append-text
            *dom-output*
            (format nil "~a generic function"
                    (slot-value (sb-mop:generic-function-method-combination (symbol-function function))
                                'sb-pcl::type-name)))))
        ((get function 'modes)
         (append-text *dom-output* "Command")
         (dolist (m (get function 'modes))
           (unless (eql m :global)
             (append-text
              *dom-output*
              (format nil " in ~a" m)))
           (when-let (bindings (collect-command-keybindings
                                function (keymap m)))
             (append-text *dom-output* " (")
             (append-child *dom-output*
                           (make-element
                            "code" :children
                            (list (sera:mapconcat
                                   #'key-description
                                   bindings ", "))))
             (append-text *dom-output* ")"))))
        (t (append-text *dom-output* "Function")))
  (when (fboundp (list 'setf function))
    (append-text *dom-output* ", setf-able"))
  (append-text *dom-output* ": "))

(defun fundoc (function)
  (let ((*print-case* :downcase))
    (let ((*dom-output*
            (append-child *dom-output* (make-element "dt"))))
      (function-short-description function)
      (append-child *dom-output*
                    (make-element "code" :children (list (prin1-to-string function))))
      (append-text *dom-output* " ")
      (append-child *dom-output*
                    (make-element "code" :children
                                  (list (print-arglist
                                         (swank-backend:arglist function)
                                         (symbol-package function))))))
    (render-doc-string (documentation function 'function))))

(defun vardoc (var)
  (let ((*print-case* :downcase))
    (let ((*dom-output* (append-child *dom-output* (make-element "dt"))))
      (append-text *dom-output* "Variable: ")
      (append-child *dom-output*
                    (make-element "code" :children (list (prin1-to-string var)))))
    (render-doc-string (documentation var 'variable))))

(defun classdoc (class)
  (let ((*print-case* :downcase)
        (object (find-class class)))
    (let ((*dom-output* (append-child *dom-output* (make-element "dt"))))
      (append-text *dom-output* "Class: ")
      (append-child *dom-output*
                    (make-element "code" :children (list (prin1-to-string class))))
      (append-text *dom-output* " inherits ")
      (append-child
       *dom-output*
       (make-element
        "code"
        :children
        (list (print-arglist
               (mapcar #'class-name
                       (sb-mop:class-direct-superclasses object))
               (symbol-package class))))))
    (render-doc-string (documentation class 'type))
    (when-let (slots (sb-mop:class-direct-slots object))
      (let ((*dom-output*
              (append-child
               (append-child *dom-output*
                             (make-element "dd"))
               (make-element "dl"))))
        (iter (for slot in slots)
          (let ((*dom-output* (append-child *dom-output* (make-element "dt"))))
            (append-text *dom-output* "Slot: ")
            (append-child
             *dom-output*
             (make-element
              "code" :children
              (list (prin1-to-string
                     (sb-mop:slot-definition-name slot)))))
            (render-doc-string (documentation slot t))))))))

(defun expand-comma-expr (node)
  (labels ((process (node)
             (if (element-p node)
                 (if (class-p node "comma-expr")
                     (progn
                       (eval (node-to-sexp (first-child node)))
                       nil)
                     (lret ((*dom-output* (clone-node node nil)))
                       (iter (for c first (first-child node)
                                  then (next-sibling c))
                         (while c)
                         (when-let (d (process c))
                           (append-child *dom-output* d)))))
                 (clone-node node nil))))
    (process node)))

(define-command render-html-doc
  :mode html-doc-mode
  (&optional
   (output-path
    (let ((path (file-path (current-buffer))))
      (make-pathname
       :directory
       (append (pathname-directory path)
               (list "build"))
       :defaults path))))
  "Render current buffer by expanding comma expressions."
  (ensure-directories-exist output-path)
  (with-open-file (s output-path
                     :direction :output
                     :if-exists :supersede)
    (message "Rendering ~a" output-path)
    (with-standard-io-syntax
      (let ((*package* (find-package "NEOMACS")))
        (serialize-document
         (expand-comma-expr (document-root (current-buffer)))
         (styles (current-buffer))
         s)))
    (message "Rendered to ~a" output-path)))

(defun build-manual-section (file)
  (with-current-buffer (find-file-no-select file)
    (render-html-doc)
    (let (title subtitles)
      (do-elements
          (lambda (node)
            (when (tag-name-p node "h1")
              (setq title (text-content node)))
            (when (tag-name-p node "h2")
              (push (text-content node) subtitles)))
        (document-root (current-buffer)))
      (values title (nreverse subtitles)))))

(defun build-manual ()
  (let ((sections '("intro" "dom" "positions" "markers"
                    "mode" "keymaps" "styles" "buffers"
                    "motion" "command-loop"
                    "edit" "undo" "ranges"
                    "window-management" "syntax")))
    (with-current-buffer
        (find-file-no-select
         (ceramic:resource 'doc "build/toc.html"))
      (erase-buffer)
      (let ((*dom-output*
              (make-element "ol")))
        (iter (for section in sections)
          (for file = (ceramic:resource
                       'doc (make-pathname :name section :type "html")))
          (for href = (str:concat section ".html"))
          (let ((*dom-output* (append-child
                               *dom-output*
                               (make-element "li"))))
            (multiple-value-bind
                  (title subtitles)
                (build-manual-section file)
              (append-child
               *dom-output*
               (make-element "a" :href href :children (list title)))
              (let ((*dom-output* (append-child *dom-output*
                                                (make-element "ul"))))
                (iter
                  (for subtitle in subtitles)
                  (append-child
                   *dom-output*
                   (make-element
                    "li" :children
                    (list (make-element
                           "a" :href
                           (str:concat href "#"
                                       (heading-text-to-id subtitle))
                           :children (list subtitle))))))))))
        (insert-nodes
         (end-pos (document-root (current-buffer)))
         *dom-output*)
        (save-buffer)
        (render-html-doc (ceramic:resource 'doc "build/index.html"))))))

(define-command manual ()
  "View Neomacs manual."
  (let ((toc-path
          (ceramic:resource 'doc "build/index.html")))
    (unless (uiop:file-exists-p toc-path)
      (if (read-yes-or-no "Manual seems not built yet, build now? ")
          (build-manual)
          (user-error "Manual not built")))
    (switch-to-buffer
     (get-buffer-create
      "*manual*" :mode 'web-mode
      :url (file-path-url toc-path)))))

(defsheet html-doc-mode
    `((":empty::after" :content "_")
      ("li p" :margin 0)
      ("body" :white-space "normal" :max-width "48rem"
              :text-align "justify" :hyphens "auto")
      (".comma-expr::before" :content ",")
      (".comma-expr" :border "solid 1px currentColor")))
