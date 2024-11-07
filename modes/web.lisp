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
    (&optional (url-or-query
                (read-from-minibuffer
                 "Find URL: ")))

  (switch-to-buffer
   (make-buffer
    "Web" :modes 'web-mode
    :url (or (look-like-url-p url-or-query)
             (str:concat *search-prefix* url-or-query))
    :styles nil)))

(defvar *web-history-list* nil)

(define-class history-entry ()
  ((title :initform nil)
   (url :initform (alex:required-argument :url) :initarg :url)
   (access-time :initform (local-time:now))))

(defmethod initialize-instance :after ((self history-entry) &key)
  (push self *web-history-list*))

(define-mode web-mode (read-only-mode)
  ((scroll-multiplier :default 16 :type (integer 1))
   (hints-selector :default "a, button, input, textarea, details, select"
                   :type string)
   (history-blocklist :default '("https://duckduckgo.com/l/"))
   (history-entry)))

(defmethod render-focus-aux ((buffer web-mode) (pos t)))

(defmethod enable-aux ((mode-name (eql 'web-mode)))
  (setf (history-entry (current-buffer))
        (make-instance 'history-entry
                       :url (url (current-buffer)))))

(define-keys global
  "C-x C-l" 'find-url)

(defnclo web-send-key-command (key) ()
  (web-send-key key))

(define-keys web-mode
  'next-line #+nil 'web-next-line
  (make-web-send-key-command
   (car (parse-keyspec "down")))
  'previous-line #+nil 'web-previous-line
  (make-web-send-key-command
   (car (parse-keyspec "up")))
  'backward-node #+nil 'web-go-backward
  (make-web-send-key-command
   (car (parse-keyspec "left")))
  'forward-node #+nil 'web-go-forward
  (make-web-send-key-command
   (car (parse-keyspec "right")))
  'scroll-up-command #+nil 'web-scroll-up
  (make-web-send-key-command
   (car (parse-keyspec "page-up")))
  'scroll-down-command #+nil 'web-scroll-down
  (make-web-send-key-command
   (car (parse-keyspec "page-down")))
  'beginning-of-buffer #+nil 'web-scroll-to-top
  (make-web-send-key-command
   (car (parse-keyspec "home")))
  'end-of-buffer #+nil 'web-scroll-to-bottom
  (make-web-send-key-command
   (car (parse-keyspec "end")))
  'self-insert-command 'web-forward-key
  "escape" 'web-forward-key
  'backward-delete
  (make-web-send-key-command
   (car (parse-keyspec "backspace"))))

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
  (setf (title (history-entry buffer)) title)
  (rename-buffer title))

(defmethod on-buffer-did-start-navigation progn
    ((buffer web-mode) url)
  (unless (or (find-if (alex:rcurry #'sera:string-prefix-p url)
                       (history-blocklist buffer))
              (equal (url buffer) url))
    (let ((old-url (url buffer)))
      (setf (history-entry buffer)
            (make-instance 'history-entry :url url))
      (record-undo
       (nclo undo-navigate ()
         (load-url buffer old-url))
       (nclo redo-navigate ()
         (load-url buffer url))
       buffer)
      (setf (url buffer) url))))

(defun key-sym-to-electron (sym shift)
  (if-let (translation (gethash (cons sym shift) *event-to-char*))
    (values (string translation) nil)
    (values sym shift)))

(defun web-send-key (key)
  (let ((shift (key-shift key)) code)
    (setf (values code shift)
          (key-sym-to-electron (key-sym key) shift))
    (evaluate-javascript
     (ps:ps
       (let ((buf (js-buffer (current-buffer)))
             (code (ps:lisp code))
             (modes
               (ps:lisp
                (let (mods)
                  (when shift
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
     :global)))

(define-command web-forward-key
  :mode web-mode ()
  (web-send-key (lastcar *this-command-keys*)))

;;; Mode hooks

(pushnew 'undo-mode (hooks 'web-mode))
