(in-package #:neomacs)

(sera:export-always
    '(*search-prefix* *web-history-path*))

(defun look-like-url-p (string)
  (ignore-errors
   (let ((parsed (quri:uri string)))
     (if (member (quri:uri-scheme parsed)
                 ;; From https://developer.mozilla.org/en-US/docs/Web/URI/Schemes
                 '("http" "https" "blob" "data" "file"
                   "javascript" "ftp" "mailto"
                   "ssh" "tel" "urn" "view-source" "ws" "wss"
                   "about")
                 :test 'equal)
         string
         (let ((prepended (str:concat "https://" string)))
           (setq parsed (quri:uri prepended))
           (when (or (quri:ip-addr-p (quri:uri-host parsed))
                     (ignore-errors
                      (cl-tld:get-tld (quri:uri-domain parsed))))
             prepended))))))

(defvar *search-prefix* "https://duckduckgo.com/html/?q="
  "URL prefix prepended to search query.")

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
    :styles nil
    :content-scripts nil)))

(define-class history-entry (bknr.datastore:store-object)
  ((title :initform nil :initarg :title)
   (url :initform (alex:required-argument :url) :initarg :url)
   (access-time :initarg :time))
  (:metaclass bknr.datastore:persistent-class)
  (:default-initargs :time (get-universal-time)))

(defun record-history-maybe (buffer url)
  (unless (and (history-entry buffer)
               (equal url (url (history-entry buffer))))
    (if (or (sera:string-prefix-p "file:" url)
            (sera:string-prefix-p "about:" url)
            (sera:string-prefix-p "neomacs:" url))
        (setf (history-entry buffer) nil)
        (setf (history-entry buffer)
              (make-instance 'history-entry :url url)))))

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

(defnclo web-send-key-command (keyspec) ()
  (iter (for key in (kbd keyspec)) (web-send-key key)))

(define-keys web-mode
  'next-line (make-web-send-key-command "down")
  'previous-line (make-web-send-key-command "up")
  'backward-node (make-web-send-key-command "left")
  'forward-node (make-web-send-key-command "right")
  'scroll-up-command (make-web-send-key-command "page-up")
  'scroll-down-command (make-web-send-key-command "page-down")
  'beginning-of-line (make-web-send-key-command "home")
  'end-of-line (make-web-send-key-command "end")
  'beginning-of-buffer (make-web-send-key-command "home")
  'end-of-buffer (make-web-send-key-command "end")
  'forward-word (make-web-send-key-command "C-right")
  'backward-word (make-web-send-key-command "C-left")
  'self-insert-command 'web-forward-key
  "processing" 'do-nothing
  "escape" 'web-forward-key
  "enter" 'web-forward-key
  "tab" 'web-forward-key
  "S-tab" 'web-forward-key
  'forward-cut (make-web-send-key-command "S-end delete")
  'backward-delete-word (make-web-send-key-command "S-C-left delete")
  'forward-delete-word (make-web-send-key-command "S-C-right delete")
  'backward-delete (make-web-send-key-command "backspace")
  'forward-delete (make-web-send-key-command "delete")
  'copy-element 'web-copy
  'cut-element 'web-cut
  'paste 'web-paste
  "C-c b" 'web-go-backward
  "C-c f" 'web-go-forward
  "C-c w" 'web-copy-url
  "C-c C-f" 'toggle-fullscreen
  "C-+" 'zoom-increase
  "C--" 'zoom-decrease
  "C-0" 'zoom-reset)

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
    (bknr.datastore:with-transaction ()
      (setf (title entry) title)))
  (rename-buffer title))

(defmethod on-buffer-did-start-navigation progn
    ((buffer web-mode) details)
  (when (assoc-value details :is-main-frame)
    ;; Events caused by history navigation and loadURL doesn't have
    ;; initiator property. We don't record them here. If some loadURL
    ;; need to be recorded, the command which invoked it is responsible for that.
    (when (assoc :initiator details)
      (record-history-maybe buffer (assoc-value details :url)))
    (setf (url buffer) (assoc-value details :url))))

(define-command web-go-backward
  :mode web-mode ()
  (setf (history-entry (current-buffer)) nil)
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
  (setf (history-entry (current-buffer)) nil)
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

(define-command web-copy-url
  :mode web-mode ()
  "Copy URL of current page into clipboard."
  (let ((url (url (current-buffer))))
    (clipboard-insert
     (list (attach-presentation
            (make-element
             "a" :href url
                 :children (list (name (current-buffer))))
            url)))
    (message "Copied ~a" url)))

;; These commands are web-mode only for now, because Chromium zoom is
;; shared for same-origin documents, and currently using them on
;; Neomacs's buffers affects all of them, causing funny.

(define-command zoom-increase
  :mode web-mode ()
  (evaluate-javascript
   (format nil "{const wc = Ceramic.buffers[~s].webContents;
wc.setZoomFactor(wc.getZoomFactor()*1.2)}"
           (id (current-buffer)))
   :global))

(define-command zoom-decrease
  :mode web-mode ()
  (evaluate-javascript
   (format nil "{const wc = Ceramic.buffers[~s].webContents;
wc.setZoomFactor(wc.getZoomFactor()/1.2)}"
           (id (current-buffer)))
   :global))

(define-command zoom-reset
  :mode web-mode ()
  (evaluate-javascript
   (format nil "{const wc = Ceramic.buffers[~s].webContents;
wc.setZoomFactor(1.0)}"
           (id (current-buffer)))
   :global))

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
    (collecting
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


(defun file-path-url (path)
  (str:concat "file://" (uiop:native-namestring path)))

(defmethod revert-buffer-aux :around ((buffer web-mode))
  (when (typep buffer 'file-mode)
    (setf (url buffer) (file-path-url (file-path buffer))))
  (load-url buffer (url buffer)))

(defun web-send-key (key)
  (evaluate-javascript
   (ps:ps
     (let ((buf (js-buffer (current-buffer)))
           (code (ps:lisp (key-sym key)))
           (mods
             (ps:lisp
              (let (mods)
                (when (key-shift key)
                  (push "shift" mods))
                (when (key-ctrl key)
                  (push "control" mods))
                (when (key-meta key)
                  (push "alt" mods))
                (when (key-super key)
                  (push "meta" mods))
                (cons 'list mods)))))
       (ps:chain buf ignore-keys
                 (push (ps:create type "keyDown"
                                  key-code code
                                  modifiers mods)))
       (ps:chain
        buf web-contents
        (send-input-event
         (ps:create type "keyDown"
                    key-code code
                    modifiers mods)))
       (ps:chain
        buf web-contents
        (send-input-event
         (ps:create type "char"
                    key-code code
                    modifiers mods)))
       (ps:chain
        buf web-contents
        (send-input-event
         (ps:create type "keyUp"
                    key-code code
                    modifiers mods)))))
   :global))

(define-command web-forward-key
  :mode web-mode ()
  (web-send-key (lastcar *this-command-keys*)))

;;; Web histroy

(define-mode web-history-list-mode (list-mode) ())

(defmethod generate-rows ((buffer web-history-list-mode))
  (iter (for entry in (sort (bknr.datastore:store-objects-with-class 'history-entry) #'>
                            :key #'access-time))
    (collecting
      (dom `(:tr (:td :class "title" ,(or (title entry) "-"))
                 (:td :class "url" ,(url entry))
                 (:td :class "time"
                      ,(format-readable-timestring
                        (local-time:universal-to-timestamp
                         (access-time entry)))))))))

(defsheet web-history-list-mode
    `((".title"
       :max-width "50vw"
       :overflow "hidden"
       :text-overflow "ellipsis")
      (".url"
       :max-width "30vw"
       :overflow "hidden"
       :text-overflow "ellipsis")))

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
