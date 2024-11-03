(in-package #:neomacs)

(define-command find-url
    (&optional (url
                (read-from-minibuffer
                 "Find URL: ")))
  (let ((parsed (quri:uri url)))
    (if (quri:uri-scheme parsed)
        (setq url parsed)
        (setq url (quri:uri (str:concat "https://duckduckgo.com/?q=" url)))))
  (switch-to-buffer
   (make-buffer "Web" :modes 'web-mode :url url
                :styles nil)))

(define-mode web-mode (read-only-mode)
  ((scroll-multiplier :default 16 :type (integer 1))
   (hints-selector :default "a, button, input, textarea, details, select"
                   :type string)))

(defmethod render-focus-aux ((buffer web-mode) (pos t)))

(define-keys global
  "C-x C-l" 'find-url)

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
   (car (parse-keyspec "backspace")))
  "C-M-f" 'web-go-forward
  "C-M-b" 'web-go-backward)

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
  (rename-buffer title))

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
       nil)
    (error "Can not go backward.")))

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
       nil)
    (error "Can not go forward.")))

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
     nil)))

(defnclo web-send-key-command (key) ()
  (web-send-key key))

(define-command web-forward-key ()
  (web-send-key (lastcar *this-command-keys*)))
