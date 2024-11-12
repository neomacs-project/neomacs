(in-package #:neomacs)

;;; Hints
;; Initially adapted from Nyxt and my XWWP fork

(define-content-script web-hints
  (defun create-hint-element (hint)
    (let ((hint-element (ps:chain document (create-element "span"))))
      (setf (ps:@ hint-element class-name) "neomacs-hint"
            (ps:@ hint-element text-content) hint)
      hint-element))

  (defun set-hint-element-style (hint-element hinted-element)
    (let ((rect (ps:chain hinted-element (get-bounding-client-rect)))
          (hinted-element-font-size (ps:@ (ps:chain window (get-computed-style hinted-element))
                                          font-size))
          (hint-font-size-lower-bound 5))
      (setf (ps:@ hint-element style position) "absolute"
            (ps:@ hint-element style top) (+ (ps:@ window scroll-y) (ps:@ rect top) "px")
            (ps:@ hint-element style left) (+ (ps:@ window scroll-x) (ps:@ rect left)
                                              "px"))
      (when (> (parse-float hinted-element-font-size)
               hint-font-size-lower-bound)
        (setf (ps:@ hint-element style font-size) hinted-element-font-size))))

  (defun create-hint-overlay (hinted-element hint)
    "Create a DOM element to be used as a hint."
    (let ((hint-element (create-hint-element hint)))
      (set-hint-element-style hint-element hinted-element))
    hint-element)

  (defun hint-label (i label-length chars)
    (ps:chain
     i (to-string (ps:chain chars length)) (split "")
     (map (lambda (char)
            (let ((code (ps:chain char (char-code-at 0))))
              (ps:chain chars (char-at
                               (- code
                                  (if (< code 97)
                                      48 87)))))))
     (join "")
     (pad-start label-length
                (ps:chain chars (char-at 0)))))

  (defun element-in-view-port-p (element)
    (let* ((rect (ps:chain element (get-bounding-client-rect)))
           (computed-style (ps:chain window (get-computed-style element))))
      (if (and (>= (ps:chain rect top) 0)
               ;; a partially visible element is still visible
               (<= (ps:chain rect top) (- (ps:chain window inner-height) 1))
               (>= (ps:chain rect left) 0)
               ;; a partially visible element is still visible
               (<= (ps:chain rect left) (- (ps:chain window inner-width) 1))
               ;; some elements have top=bottom=left=right
               (> (ps:chain rect width) 3)
               (> (ps:chain rect height) 3)
               (not (= (ps:chain computed-style "visibility") "hidden"))
               (not (= (ps:chain computed-style "display") "none")))
          t nil)))

  (defun hint-elements (css chars)
    (let* ((hints-parent (ps:chain document (create-element "div")))
           (shadow (ps:chain hints-parent (attach-shadow (ps:create mode "open"))))
           (candidates
             (ps:chain
              *array
              (from (ps:chain
                     document
                     (query-selector-all
                      "a, button, input, textarea, details, select")))
              (filter #'element-in-view-port-p)))
           (label-length (ps:chain
                          *math
                          (ceil (/ (ps:chain *math (log (ps:chain candidates length)))
                                   (ps:chain *math (log 26))))))
           (i 0))
      (dolist (hinted-element candidates)
        (let ((hint (hint-label i label-length chars)))
          (ps:chain hinted-element (set-attribute "neomacs-hint" hint))
          (ps:chain shadow (append-child (create-hint-overlay hinted-element hint)))
          (incf i)))
      (let ((style (ps:new (|CSSStyleSheet|))))
        (ps:chain style (replace-sync css))
        (setf (ps:chain shadow adopted-style-sheets) (array style)))
      (setf (ps:@ hints-parent id) "neomacs-hints"
            (ps:@ hints-parent style) "all: unset !important;")
      ;; Unless the hints root is a child of body, zooming the page breaks the
      ;; hint positioning.
      (ps:chain document body (append-child hints-parent))
      label-length)))

(define-mode active-web-hint-mode ()
  ((label-length)
   (label-keys :initform "")
   (ctrl-p :initform nil :initarg :ctrl))
  (:documentation "Transient mode when selecting web hints."))

(define-keys active-web-hint-mode
  'keyboard-quit 'remove-hints)

(iter (for i from (char-code #\a) to (char-code #\z))
  (for char = (code-char i))
  (set-key (keymap 'active-web-hint-mode)
           (string char) 'narrow-hint))

(define-keys web-mode
  "M-g" 'add-hints
  "M-G" 'add-hints-ctrl)

(defvar *hints-chars* "asdfghjklqwertyuiopzxcvbnm")

(define-command add-hints
  :mode web-mode ()
  "Select visible interactive elements using hints."
  (pushnew 'web-hints (content-scripts (current-buffer)))
  (let ((length (evaluate-javascript-sync
                 (ps:ps (hint-elements
                         (ps:lisp (cell-ref (css-cell 'web-hints)))
                         "asdfghjkl"))
                 (current-buffer))))
    (enable 'active-web-hint-mode)
    (setf (label-length (current-buffer)) length)))

(define-command add-hints-ctrl
  :mode web-mode ()
  "Like `add-hints', but simulate ctrl click."
  (add-hints)
  (setf (ctrl-p (current-buffer)) t))

(defun remove-hints ()
  (evaluate-javascript
   (ps:ps
     (ps:let ((hints-parent (ps:chain document (get-element-by-id "neomacs-hints"))))
       (ps:when hints-parent
         (ps:chain hints-parent (remove)))))
   (current-buffer))
  (disable 'active-web-hint-mode))

(defun narrow-hint ()
  (let ((key (key-description (lastcar *this-command-keys*))))
    (setf (label-keys (current-buffer))
          (str:concat (label-keys (current-buffer)) key))
    (if (< (length (label-keys (current-buffer)))
           (label-length (current-buffer)))
        (evaluate-javascript
         (ps:ps
           (ps:let ((hints-parent (ps:chain document (get-element-by-id "neomacs-hints"))))
             (ps:chain *array (from (ps:chain hints-parent shadow-root children))
                       (for-each
                        (lambda (element)
                          (if (ps:chain element text-content (starts-with (ps:lisp key)))
                              (setf (ps:chain element text-content)
                                    (ps:chain element text-content
                                              (substring 1)))
                              (ps:chain element (remove))))))))
         (current-buffer))
        (progn
          (evaluate-javascript
           (ps:ps
             (let ((element (ps:chain
                             document
                             (query-selector
                              (ps:lisp (format
                                        nil "[neomacs-hint=~S]"
                                        (label-keys (current-buffer))))))))
               (ps:chain element (focus))
               (set-timeout
                (lambda ()
                  (ps:chain
                   element
                   (dispatch-event
                    (ps:new
                     (-mouse-event
                      "click"
                      (ps:create
                       ctrl-key (ps:lisp (ctrl-p (current-buffer)))))))))
                0)))
           (current-buffer))
          (remove-hints)))))

(defstyle web-hints `((".neomacs-hint"
                       :background-color "#fff"
                       :padding "0px 0.3em"
                       :border-radius "2px"
                       :border-width "2px"
                       :border-style "solid"
                       :z-index #.(1- (expt 2 31)))))
