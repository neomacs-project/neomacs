(in-package #:neomacs)

;;; TAB completion

(define-class active-completion-mode ()
  ((current-completion :initform 0)
   (completions :initform nil)
   (replace-range))
  (:documentation "Transient mode when completion menu is active."))

(define-keymap active-completion-mode ()
  "up" 'previous-completion
  "down" 'next-completion
  "C-p" 'previous-completion
  "C-n" 'next-completion
  "C-v" 'scroll-completion-down
  "M-v" 'scroll-completion-up
  "C-g" 'hide-completions
  "enter" 'complete-selection)

(defgeneric compute-completion (buffer pos))

(define-command show-completions (&optional (marker (focus)) silent)
  (bind ((buffer (current-buffer))
         ((replace-range completions)
          (compute-completion (current-buffer) (pos marker))))
    (unless completions
      (unless silent (message "No completion."))
      (return-from show-completions))
    (labels ((emit-match (submatch)
               (spinneret:with-html
                 (:span :class "completion-match"
                        submatch)))
             (emit-row (string matches attr)
               (spinneret:with-html
                 (:tr (:td (iter (with current-position = 0)
                             (for (position submatch) in matches)
                             (when (> position current-position)
                               (write-string (subseq string current-position position)
                                             spinneret:*html*))
                             (emit-match submatch)
                             (setf current-position (+ position (length submatch)))
                             (finally
                              (when (< current-position (length string))
                                (write-string (subseq string current-position)
                                              spinneret:*html*)))))
                      (:td :class "completion-annotation" (remove #\- attr))))))
      (send-dom-update
       `(let ((menu (ps:chain document (get-element-by-id "completion-menu")))
              (rect (pos-bounding-rect ,(range-end replace-range))))
          (unless menu
            (let ((table (ps:chain document (create-element "table"))))
              (setq menu (ps:chain document (create-element "tbody")))
              (setf (ps:chain menu id) "completion-menu")
              (ps:chain table (append-child menu))
              (ps:chain document body (append-child table))))
          (setf (ps:chain menu inner-h-t-m-l)
                ,(spinneret:with-html-string
                   (iter (for c in completions)
                     (for string = (car c))
                     (for matches = (caddr c))
                     (emit-row string matches
                               (string-upcase (cadddr c))))))
          (setf (ps:chain menu style display) "block"
                (ps:chain menu style left)
                (+ (ps:chain rect right) (ps:chain window scroll-x) "px")
                (ps:chain menu style top)
                (+ (ps:chain rect bottom) (ps:chain window scroll-y) "px"))
          nil)
       (current-buffer)))
    (enable 'active-completion-mode)
    (setf (completions buffer) completions
          (current-completion buffer) 0
          (replace-range buffer) replace-range)))

(defun maybe-hide-completions ()
  (let ((buffer (current-buffer)))
    (unless
        (or (inside-range-p (focus) (replace-range buffer))
            (inside-range-p (npos-prev-until (focus) #'selectable-p)
                            (replace-range buffer)))
      (hide-completions))))

(define-command hide-completions ()
  (disable 'active-completion-mode))

(defmethod on-post-command progn ((buffer active-completion-mode))
  (maybe-hide-completions))

(defmethod disable-aux ((mode (eql 'active-completion-mode)))
  (send-dom-update
   `(setf (ps:chain document (get-element-by-id "completion-menu")
                    style display)
          "none")
   (current-buffer)))

(defmethod (setf current-completion) :after (new-value (buffer active-completion-mode))
  (send-dom-update
   `(let ((menu (ps:chain document (get-element-by-id "completion-menu"))))
      (dolist (e (ps:chain document (get-elements-by-class-name "completion-selection")))
        (ps:chain e class-list (remove "completion-selection")))
      (let ((selection (aref (ps:chain menu child-nodes) ,new-value)))
        (ps:chain selection class-list (add "completion-selection"))
        (ps:chain selection (scroll-into-view-if-needed)))
      nil)
   buffer))

#+nil (define-command next-completion (&optional (buffer (current-buffer)))
  (if (< (1+ (current-completion buffer)) (length (completions buffer)))
      (incf (current-completion buffer))
      (message "No next completion.")))

#+nil (define-command previous-completion (&optional (buffer (current-buffer)))
  (if (> (current-completion buffer) 0)
      (decf (current-completion buffer))
      (message "No previous completion.")))

#+nil (define-command scroll-completion-down (&optional (buffer (current-buffer)))
  (setf (current-completion buffer)
        (min (+ (current-completion buffer) (scroll-lines buffer))
             (1- (length (completions buffer))))))

#+nil (define-command scroll-completion-up (&optional (buffer (current-buffer)))
  (setf (current-completion buffer)
        (max (- (current-completion buffer) (scroll-lines buffer))
             0)))

(define-command complete-selection ()
  (bind ((buffer (current-buffer))
         (selection (nth (current-completion buffer) (completions buffer)))
         (range (replace-range buffer))
         (end (range-end range)))
    (disable 'active-completion-mode)
    (with-marker (marker end)
      (delete-range range)
      (insert-nodes marker (car selection)))))

;;; Auto completion

#+nil (define-mode auto-completion-mode ()
  "Automatically show completion menu after input."
  ((minimum-prefix 3)
   (allowed-commands '(self-insert) :type (list-of symbol))))

#+nil (defun auto-completion-post-update ()
  (let ((mode (find-submode 'auto-completion-mode)))
    (when-let (node (symbol-around (focus)))
      (when (and
             (member (name (last-command *browser*))
                     (allowed-commands mode))
             (>= (length (text (first-child node)))
                 (minimum-prefix mode)))
        (show-completions (focus) t)))))

#+nil (defmethod enable ((mode auto-completion-mode) &key)
  (hooks:add-hook (post-command-hook (find-submode 'neomacs-mode))
                  'auto-completion-post-update))

#+nil (defmethod disable ((mode auto-completion-mode) &key)
  (when-let (neomacs (find-submode 'neomacs-mode))
    (hooks:remove-hook (post-command-hook neomacs) 'auto-completion-post-update)))

;;; Style

(defstyle completion-menu `(:width "auto"
                            :white-space "nowrap"
                            :position "absolute"
                            :z-index 10
                            :background-color "#fff"

                                   ;; :border "1px solid #bde1ff"
                            :font-size "0.8em"
                            :color "#777"
                            :box-shadow "0 0 0.4em"
                                   ;; :max-width "20em"
                            :max-height "30em"
                                   ;; :overflow-x "clip"
                            :overflow-y "scroll"
                            :border-collapse "collapse"))
(defstyle completion-match `(:color "#000"))
(defstyle completion-annotation `(:text-align "right"
                                  :font-style "italic"))
(defstyle completion-selection `(:inherit focus))

(defstyle completion
    `(("#completion-menu" :inherit completion-menu)
      (".completion-match" :inherit completion-match)
      (".completion-annotation" :inherit completion-annotation)
      (".completion-selection" :inherit completion-selection)))
