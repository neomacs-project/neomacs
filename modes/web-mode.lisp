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

(define-class web-mode ()
  ((scroll-multiplier :default 16 :type (integer 1))))

(defmethod render-focus-aux ((buffer web-mode) (pos t)))

(define-keys global
  "C-x C-l" 'find-url)

(define-keymap web-mode ()
  'next-line 'web-next-line
  'previous-line 'web-previous-line
  'scroll-up-command 'web-scroll-up
  'scroll-down-command 'web-scroll-down
  'beginning-of-buffer 'web-scroll-to-top
  'end-of-buffer 'web-scroll-to-bottom)

(define-command web-next-line ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (scroll-multiplier (current-buffer))))))
   (current-buffer)))

(define-command web-previous-line ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (- (scroll-multiplier (current-buffer)))))))
   (current-buffer)))

(define-command web-scroll-up ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (* (- (scroll-lines (current-buffer)))
                         (scroll-multiplier (current-buffer)))))))
   (current-buffer)))

(define-command web-scroll-down ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:lisp
                      (* (scroll-lines (current-buffer))
                         (scroll-multiplier (current-buffer)))))))
   (current-buffer)))

(define-command web-scroll-to-top ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (- (ps:chain document document-element
                                  scroll-height)))))
   (current-buffer)))

(define-command web-scroll-to-bottom ()
  (evaluate-javascript
   (ps:ps (ps:chain
           window (scroll-by
                   0 (ps:chain document document-element
                               scroll-height))))
   (current-buffer)))

(defmethod on-buffer-title-updated progn ((buffer web-mode) title)
  (rename-buffer title))
