(in-package #:neomacs)

(sera:export-always
    '(compute-completion completions replace-range))

;;; TAB completion

(define-mode active-completion-mode (completion-mode)
  ((replace-range)
   (menu-node)
   (completions)
   (completion-selection :initform 0))
  (:documentation "Transient mode when completion menu is active."))

(define-keys prog-mode
  "tab" 'show-completions)

(define-keys active-completion-mode
  'next-line 'next-buffer-completion
  'previous-line 'previous-buffer-completion
  'scroll-down-command 'scroll-down-buffer-completion
  'scroll-up-command 'scroll-up-buffer-completion
  'beginning-of-buffer 'first-buffer-completion
  'end-of-buffer 'last-buffer-completion
  "C-g" 'hide-completions
  "enter" 'complete-selection)

(defgeneric compute-completion (buffer pos)
  (:documentation
   "Compute completions for BUFFER at POS.

Should return two values: REPLACE-RANGE, COMPLETIONS.

REPLACE-RANGE is a range which may be replaced by some completion.

COMPLETIONS is a list of completions. Each completion is a list of
form (text annotation)."))

(define-command show-completions (&optional (marker (focus)) silent)
  (bind ((buffer (current-buffer))
          ((:values replace-range completions)
           (compute-completion buffer marker)))
    (unless completions
      (unless silent (message "No completion."))
      (hide-completions)
      (return-from show-completions))
    (enable 'active-completion-mode)
    (setf (replace-range buffer) replace-range
          (completions buffer) completions
          (completion-selection buffer) 0
          (menu-node buffer)
          (dom `(:table :class "completion-menu"
                 (:tbody ,@ (iter (for (c annot) in completions)
                              (collecting
                                `(:tr :class ,(if (first-iteration-p) "completion-selection" "")
                                  (:td :class "completion-candidate"
                                       ,c)
                                  (:td :class "completion-annotation"
                                       ,@(when (> (length annot) 0)
                                           (list annot))))))))))
    (evaluate-javascript
     (format nil "{var menu = document.getElementById('neomacs-completion-menu');
if(!menu){
    menu = document.createElement('table');
    menu.id = 'neomacs-completion-menu';
    document.documentElement.appendChild(menu)}
menu.innerHTML = '~A';}"
             (quote-js (serialize (first-child (menu-node buffer)) nil)))
     buffer)
    (run-in-helper '*window-layout-helper* 'update-completion-menu-position buffer)
    t))

(defmethod (setf completion-selection) :before (new-val (buffer active-completion-mode))
  (let ((old-val (slot-value buffer 'completion-selection)))
    (unless (eql old-val new-val)
      (evaluate-javascript
       (format nil "{const menu = document.getElementById('neomacs-completion-menu');
menu.firstChild.childNodes[~a].classList.remove('completion-selection');
const selected = menu.firstChild.childNodes[~a];
selected.classList.add('completion-selection');
selected.scrollIntoViewIfNeeded()}"
               old-val new-val)
       buffer))))

(defun update-completion-menu-position (buffer)
  (bind (((x y w h) (get-bounding-client-rect (focus buffer))))
    (evaluate-javascript
     (format nil "{const menu = document.getElementById('neomacs-completion-menu');
if(menu){
    const rect = document.documentElement.getBoundingClientRect();
    const mw = menu.getBoundingClientRect().width;
    const mh = 300;
    const x = ~a; const y = ~a; const w = ~a; const h = ~a;
    menu.style.left = Math.max(0, Math.min(x+w, rect.width - mw)) - rect.left + 'px';
    if(y > mh && y+h+mh > rect.height){
        menu.style.top = y - mh + rect.top + 'px';
    }
    else {
        menu.style.top = y + h - rect.top + 'px';
        menu.style.bottom = '0px';
    }}}"
             x y w h)
     buffer)))

(defun maybe-hide-completions ()
  "Hide completions if focus move out of `replace-range'.

Return t if it hides completion, nil if it does nothing."
  (let ((buffer (current-buffer)))
    (when (typep buffer 'active-completion-mode)
      (unless (inside-range-inclusive-p
               (focus) (replace-range buffer))
        (hide-completions)
        t))))

(define-command hide-completions ()
  (disable 'active-completion-mode))

(defmethod on-post-command progn ((buffer active-completion-mode))
  (unless (maybe-hide-completions)
    (run-in-helper '*window-layout-helper* 'update-completion-menu-position buffer)))

(defmethod disable-aux ((mode (eql 'active-completion-mode)) buffer)
  (evaluate-javascript
   "{const menu = document.getElementById('neomacs-completion-menu');
if(menu) menu.remove()}"
   buffer))

(define-command complete-selection ()
  (bind ((buffer (current-buffer))
          (selection (car (nth (completion-selection buffer) (completions buffer))))
         (range (replace-range buffer))
         (end (range-end range)))
    (disable 'active-completion-mode)
    (with-marker (marker end)
      (delete-range range)
      (insert-nodes marker selection))))

(define-command next-buffer-completion
  :mode active-completion-mode ()
  (if (< (1+ (completion-selection (current-buffer)))
         (length (completions (current-buffer))))
      (incf (completion-selection (current-buffer)))
      (user-error "No next completion")))

(define-command previous-buffer-completion
  :mode active-completion-mode ()
  (if (>= (1- (completion-selection (current-buffer))) 0)
      (decf (completion-selection (current-buffer)))
      (user-error "No previous completion")))

(define-command scroll-down-buffer-completion
  :mode active-completion-mode ()
  (setf (completion-selection (current-buffer))
        (min (1- (length (completions (current-buffer))))
             (+ (completion-selection (current-buffer))
                (scroll-lines (current-buffer))))))

(define-command scroll-up-buffer-completion
  :mode active-completion-mode ()
  (setf (completion-selection (current-buffer))
        (max 0
             (- (completion-selection (current-buffer))
                (scroll-lines (current-buffer))))))

(define-command first-buffer-completion
  :mode active-completion-mode ()
  (setf (completion-selection (current-buffer)) 0))

(define-command last-buffer-completion
  :mode active-completion-mode ()
  (setf (completion-selection (current-buffer))
        (1- (length (completions (current-buffer))))))

;;; Auto completion

(define-mode auto-completion-mode ()
  ((minimum-prefix :default 3)
   (allowed-commands
    :default '(self-insert-command)
    :type (list-of symbol)))
  (:documentation
   "Automatically show completion menu after input.")
  (:toggler t))

(defmethod on-post-command progn ((buffer auto-completion-mode))
  (when (and (member *this-command* (allowed-commands buffer))
             (eq buffer (focused-buffer)))
    (when-let (node (node-containing (focus buffer)))
      (when (and (>= (length (text-content node))
                     (minimum-prefix buffer))
                 (show-completions (focus buffer) t))
        (update-completion-menu-position buffer)))))

;;; Style

(defstyle completion-menu
    `(:white-space "nowrap"
      :overflow-x "hidden"
      :backdrop-filter "blur(10px)"
      :overflow-y "scroll"
      :position "absolute"
      :max-height "300px"
      :padding "0 8px"
      :display "block"
      :border-collapse "collapse"))

(defstyle completion-selection `(:inherit focus))
(defstyle completion-candidate nil)
(defstyle completion-annotation `(:text-align "right"
                                  :font-style "italic"))
(defsheet active-completion-mode
    `(("#neomacs-completion-menu" :inherit (default completion-menu))
      (".completion-selection" :inherit completion-selection)
      (".completion-candidate" :inherit completion-candidate)
      (".completion-annotation" :inherit completion-annotation)))
