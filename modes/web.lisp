(in-package #:neomacs)

(defun look-like-url-p (string)
  (ignore-errors
   (let ((parsed (quri:uri string)))
     (if (quri:uri-scheme parsed)
         string
         (let ((prepended (str:concat "https://" string)))
           (setq parsed (quri:uri prepended))
           (when (or (quri:ip-addr-p (quri:uri-host parsed))
                     (ignore-errors
                      (cl-tld:get-tld (quri:uri-domain parsed))))
             prepended))))))

(defvar *search-prefix* "https://duckduckgo.com/html/?q=")

(define-command find-url
  :interactive
  (lambda ()
    (list (read-from-minibuffer
           "Find URL: " :mode 'minibuffer-find-link-mode
           :completion-buffer
           (make-completion-buffer
            '(web-history-list-mode completion-buffer-mode)
            :require-match nil))))
  (url-or-query)
  "Open or search for URL-OR-QUERY."
  (switch-to-buffer
   (make-buffer
    "Web" :mode 'web-mode
    :url (or (look-like-url-p url-or-query)
             (str:concat *search-prefix* url-or-query))
    :styles nil)))

(defvar *web-history-list* nil)

(defvar *web-history-path* nil)

(define-class history-entry ()
  ((title :initform nil :initarg :title)
   (url :initform (alex:required-argument :url) :initarg :url)
   (access-time :initform (local-time:now))))

(defmethod initialize-instance :after ((self history-entry) &key access-time)
  (push self *web-history-list*)
  (when access-time
    (setf (access-time self)
          (local-time:parse-timestring access-time))))

(defun record-history-maybe (buffer url)
  (unless (and (history-entry buffer)
               (equal url (url (history-entry buffer))))
    (if (or (sera:string-prefix-p "file" url)
            (sera:string-prefix-p "about:" url))
        (setf (history-entry buffer) nil)
        (setf (history-entry buffer)
              (make-instance 'history-entry :url url)))))

(defun load-web-history ()
  (message "Loading web history...")
  (setq *web-history-list* nil)
  (unless *web-history-path*
    (setq *web-history-path* (uiop:xdg-data-home "neomacs" "web-history")))
  (ensure-directories-exist *web-history-path*)
  (with-open-file (s *web-history-path*
                     :direction :input
                     :if-does-not-exist :create)
    (handler-case
        (loop
          (apply #'make-instance 'history-entry (read s)))
      (end-of-file () nil)))
  (message "Loaded web history."))

(defun save-web-history ()
  (unless *web-history-path*
    (setq *web-history-path* (uiop:xdg-data-home "neomacs" "web-history")))
  (ensure-directories-exist *web-history-path*)
  (with-open-file (s *web-history-path*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (with-standard-io-syntax
      (dolist (h (reverse *web-history-list*))
        (write (list :url (url h)
                     :title (title h)
                     :access-time
                     (local-time:format-timestring
                      nil (access-time h)))
               :stream s)
        (terpri s)))))

(define-mode web-mode (read-only-mode)
  ((scroll-multiplier :default 16 :type (integer 1))
   (hints-selector :default "a, button, input, textarea, details, select"
                   :type string)
   (history-entry :initform nil)))

(defmethod render-focus-aux ((buffer web-mode) (pos t)))

(defmethod enable-aux ((mode-name (eql 'web-mode)))
  (record-history-maybe
   (current-buffer)
   (url (current-buffer))))

(define-keys :global
  "C-x C-l" 'find-url)

(defnclo web-send-key-command (key) ()
  (web-send-key key))

(define-keys web-mode
  'next-line #+nil 'web-next-line
  (make-web-send-key-command
   (car (kbd "down")))
  'previous-line #+nil 'web-previous-line
  (make-web-send-key-command
   (car (kbd "up")))
  'backward-node #+nil 'web-go-backward
  (make-web-send-key-command
   (car (kbd "left")))
  'forward-node #+nil 'web-go-forward
  (make-web-send-key-command
   (car (kbd "right")))
  'scroll-up-command #+nil 'web-scroll-up
  (make-web-send-key-command
   (car (kbd "page-up")))
  'scroll-down-command #+nil 'web-scroll-down
  (make-web-send-key-command
   (car (kbd "page-down")))
  'beginning-of-buffer #+nil 'web-scroll-to-top
  (make-web-send-key-command
   (car (kbd "home")))
  'end-of-buffer #+nil 'web-scroll-to-bottom
  (make-web-send-key-command
   (car (kbd "end")))
  'self-insert-command 'web-forward-key
  "escape" 'web-forward-key
  "enter" 'web-forward-key
  'backward-delete
  (make-web-send-key-command
   (car (kbd "backspace")))
  'copy-element 'web-copy
  'cut-element 'web-cut
  'paste 'web-paste
  "C-c b" 'web-go-backward
  "C-c f" 'web-go-forward)

(define-command web-next-line
  :mode web-mode ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (scroll-multiplier (current-buffer))))))
   (current-buffer)))

(define-command web-previous-line
  :mode web-mode ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (- (scroll-multiplier (current-buffer)))))))
   (current-buffer)))

(define-command web-scroll-up
  :mode web-mode ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (* (- (scroll-lines (current-buffer)))
                         (scroll-multiplier (current-buffer)))))))
   (current-buffer)))

(define-command web-scroll-down
  :mode web-mode ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (* (scroll-lines (current-buffer))
                         (scroll-multiplier (current-buffer)))))))
   (current-buffer)))

(define-command web-scroll-to-top
  :mode web-mode ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (- (ps:chain document document-element
                                  scroll-height)))))
   (current-buffer)))

(define-command web-scroll-to-bottom
  :mode web-mode ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:chain document document-element
                               scroll-height))))
   (current-buffer)))

(defmethod on-buffer-title-updated progn ((buffer web-mode) title)
  (when-let (entry (history-entry buffer))
    (setf (title entry) title))
  (rename-buffer title))

(defmethod on-buffer-did-start-navigation progn
    ((buffer web-mode) details)
  ;; Events caused by history navigation and loadURL doesn't have
  ;; initiator property. We don't record them here. If some loadURL
  ;; need to be recorded, the command which invoked it is responsible for that.
  (if (assoc :initiator details)
      (record-history-maybe buffer (assoc-value details :url))
      (setf (history-entry buffer) nil))
  (setf (url buffer) (assoc-value details :url)))

(define-command web-go-backward
  :mode web-mode ()
  (unless
      (evaluate-javascript-sync
       (ps:ps
         (let ((h (ps:chain (js-buffer (current-buffer))
                            web-contents navigation-history)))
           (when (ps:chain h (can-go-back))
             (ps:chain h (go-back))
             t)))
       :global)
    (user-error "Can not go backward.")))

(define-command web-go-forward
  :mode web-mode ()
  (unless
      (evaluate-javascript-sync
       (ps:ps
         (let ((h (ps:chain (js-buffer (current-buffer))
                            web-contents navigation-history)))
           (when (ps:chain h (can-go-forward))
             (ps:chain h (go-forward))
             t)))
       :global)
    (user-error "Can not go forward.")))

(macrolet ((define-web-command (operation)
             `(define-command ,(alex:symbolicate "WEB-" operation)
                :mode web-mode ()
                (evaluate-javascript
                 (ps:ps (ps:chain (js-buffer (current-buffer))
                                  web-contents (,operation)))
                 :global))))
  (define-web-command copy)
  (define-web-command cut)
  (define-web-command paste))

(define-mode web-buffer-history-list-mode (list-mode)
  ((items :initform (alex:required-argument :items)
          :initarg :items)))

(defmethod generate-rows
    ((buffer web-buffer-history-list-mode))
  (iter (for item in (items buffer))
    (for i from 0)
    (insert-nodes
     (focus)
     (attach-presentation
      (dom `(:tr (:td ,(or (assoc-value item :title)
                           "-"))
                 (:td ,(or (assoc-value item :url)
                           "-"))))
      i))))

(define-command web-go-history
  :mode web-mode
  :interactive
  (lambda ()
    (list
     (completing-read
      "Go to history:"
      'web-buffer-history-list-mode
      :items
      (evaluate-javascript-sync
       (ps:ps
         (let ((h (ps:chain (js-buffer (current-buffer))
                            web-contents navigation-history))
               (result (list)))
           (dotimes (i (ps:chain h (length)))
             (ps:chain result (push (ps:chain h (get-entry-at-index i)))))
           result))
       :global))))
  (index)
  (unless
      (evaluate-javascript-sync
       (ps:ps
         (let ((h (ps:chain (js-buffer (current-buffer))
                            web-contents navigation-history)))
           (when (ps:chain h (can-go-to-offset
                              (- (ps:lisp index)
                                 (ps:chain h (get-active-index)))))
             (ps:chain h (go-to-index (ps:lisp index)))
             t)))
       :global)
    (user-error "Can not go to history entry ~a." index)))


(defmethod revert-buffer-aux ((buffer web-mode))
  (load-url buffer (url buffer)))

(defun web-send-key (key)
  (evaluate-javascript
   (ps:ps
     (let ((buf (js-buffer (current-buffer)))
           (code (ps:lisp (key-sym key)))
           (modes
             (ps:lisp
              (let (mods)
                (when (key-shift key)
                  (push "Shift" mods))
                (when (key-ctrl key)
                  (push "Control" mods))
                (when (key-meta key)
                  (push "Alt" mods))
                (when (key-super key)
                  (push "Meta" mods))
                (cons 'list mods)))))
       (ps:chain buf ignore-keys
                 (push (ps:create type "keyDown"
                                  key (ps:lisp (key-sym key)))))
       (ps:chain
        buf web-contents
        (send-input-event
         (ps:create type "keyDown"
                    key-code code)))
       (ps:chain
        buf web-contents
        (send-input-event
         (ps:create type "char"
                    key-code code)))
       (ps:chain
        buf web-contents
        (send-input-event
         (ps:create type "keyUp"
                    key-code code)))))
   :global))

(define-command web-forward-key
  :mode web-mode ()
  (web-send-key (lastcar *this-command-keys*)))

;;; Web histroy

(define-mode web-history-list-mode (list-mode) ())

(defmethod generate-rows ((buffer web-history-list-mode))
  (iter (for entry in *web-history-list*)
    (insert-nodes (focus)
                  (dom `(:tr (:td ,(or (title entry) "-"))
                             (:td ,(url entry))
                             (:td ,(format-readable-timestring
                                    (access-time entry))))))))

(define-command list-web-history ()
  (switch-to-buffer
   (get-buffer-create "*web-history*"
                      :mode 'web-history-list-mode
                      :revert t)))

(defmethod occur-p-aux ((buffer web-history-list-mode)
                        query element)
  (search-in-elements
   query (list (first-child element)
               (next-sibling (first-child element)))))

(define-mode minibuffer-find-link-mode
    (minibuffer-completion-mode) ())

(defmethod complete-minibuffer-aux
    ((buffer minibuffer-find-link-mode))
  (let ((input (only-elt (get-elements-by-class-name
                          (document-root buffer) "input")))
        (selection (node-after (focus (completion-buffer buffer)))))
    (unless (class-p selection "dummy-row")
      (delete-nodes (pos-down input) nil)
      (insert-nodes (pos-down input)
                    (text-content (next-sibling
                                   (first-child selection)))))))

;;; Find in page

(define-mode minibuffer-web-search-mode (minibuffer-mode)
  ((for-buffer :initform (alex:required-argument :buffer)
               :initarg :buffer)
   (minimum-search-prefix :default 3 :type (integer 1))))

(define-keys web-mode
  'search-forward 'web-search-forward
  'search-backward 'web-search-backward)

(define-keys minibuffer-web-search-mode
  'search-forward 'web-search-forward
  'search-backward 'web-search-backward)

(defun start-web-search ()
  (unwind-protect
       (read-from-minibuffer
        "Search: " :mode 'minibuffer-web-search-mode
                   :buffer (current-buffer))
    (evaluate-javascript
     (ps:ps
       (ps:chain (js-buffer (current-buffer))
                 web-contents (stop-find-in-page "clearSelection")))
     :global)))

(defun update-web-search (query forward)
  (evaluate-javascript
   (ps:ps
     (ps:chain (js-buffer (for-buffer (current-buffer)))
               web-contents
               (find-in-page
                (ps:lisp query)
                (ps:create forward (ps:lisp (if forward t 'ps:false))))))
   :global))

(define-command web-search-forward
  :mode (web-mode minibuffer-web-search-mode) ()
  (if (typep (current-buffer) 'minibuffer-web-search-mode)
      (let ((query (minibuffer-input (current-buffer))))
        (when (plusp (length query))
          (update-web-search query t)))
      (start-web-search)))

(define-command web-search-backward
  :mode (web-mode minibuffer-web-search-mode) ()
  (if (typep (current-buffer) 'minibuffer-web-search-mode)
      (let ((query (minibuffer-input (current-buffer))))
        (when (plusp (length query))
          (update-web-search query nil)))
      (start-web-search)))

(defmethod on-post-command progn ((buffer minibuffer-web-search-mode))
  (unless (member *this-command*
                  '(web-search-forward web-search-backward
                    exit-recursive-edit exit-minibuffer))
    (let ((query (minibuffer-input buffer)))
      (when (>= (length query) (minimum-search-prefix buffer))
        (update-web-search query t)))))
