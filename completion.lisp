(in-package #:neomacs)

;;; TAB completion

(define-mode active-completion-mode ()
  "Transient mode when completion menu is active."
  ((scroll-distance 10)
   (current-completion 0)
   (completions nil)
   (replace-range)
   (keyscheme-map
    (keymaps:define-keyscheme-map "active-completion" ()
      keyscheme:default
      '("up" previous-completion
        "down" next-completion)
      keyscheme:emacs
      '("C-p" previous-completion
        "C-n" next-completion
        "C-v" scroll-completion-down
        "M-v" scroll-completion-up
        "C-g" hide-completions
        "return" complete-selection)))
   (rememberable-p nil))
  (:toggler-command-p nil))

(define-command show-completions (&optional (marker (focus)) silent)
  (bind (((replace-range completions)
          (hooks:run-hook (completion-hook (host marker))
                          (pos marker))))
    (unless completions
      (unless silent (echo "No completion."))
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
          nil)))
    (unless (find-submode 'active-completion-mode)
      (enable-modes* 'active-completion-mode (current-buffer)))
    (let ((mode (find-submode 'active-completion-mode)))
      (setf (completions mode) completions
            (current-completion mode) 0
            (replace-range mode) replace-range))))

(defun inside-range-p (pos range)
  (bind (((beg . length) range))
    (and (text-pos-p beg) (text-pos-p pos)
         (eql (text-pos-node beg) (text-pos-node pos))
         (< (text-pos-offset beg) (text-pos-offset pos))
         (or (not length)
             (< (text-pos-offset pos) (+ (text-pos-offset beg) length))))))

(defun range-end (range)
  (bind (((beg . length) range))
    (ematch beg
      ((text-pos node offset)
       (if length
           (text-pos node (+ length offset))
           (text-pos node (length (text node))))))))

(defun maybe-hide-completions ()
  (let ((mode (find-submode 'active-completion-mode)))
    (unless (inside-range-p (npos-prev-until (focus) #'selectable-p)
                            (replace-range mode))
      (hide-completions))))

(define-command hide-completions ()
  (disable (find-submode 'active-completion-mode)))

(defmethod enable ((mode active-completion-mode) &key)
  (nhooks:add-hook (post-command-hook (find-submode 'neomacs-mode (buffer mode)))
                   'maybe-hide-completions))

(defmethod disable ((mode active-completion-mode) &key)
  (nhooks:remove-hook (post-command-hook (find-submode 'neomacs-mode (buffer mode)))
                      'maybe-hide-completions)
  (send-dom-update
   `(setf (ps:chain document (get-element-by-id "completion-menu")
                    style display)
          "none")))

(defun update-selection (buffer index)
  (with-current-buffer buffer
    (send-dom-update
     `(let ((menu (ps:chain document (get-element-by-id "completion-menu"))))
        (dolist (e (ps:chain document (get-elements-by-class-name "completion-selection")))
          (ps:chain e class-list (remove "completion-selection")))
        (let ((selection (aref (ps:chain menu child-nodes) ,index)))
          (ps:chain selection class-list (add "completion-selection"))
          (ps:chain selection (scroll-into-view-if-needed)))
        nil))))

(defmethod (setf current-completion) :after (new-value (mode active-completion-mode))
  (update-selection (buffer mode) new-value))

(define-command next-completion
    (&optional (mode (find-submode 'active-completion-mode)))
  (if (< (1+ (current-completion mode)) (length (completions mode)))
      (incf (current-completion mode))
      (echo "No next completion.")))

(define-command previous-completion
    (&optional (mode (find-submode 'active-completion-mode)))
  (if (> (current-completion mode) 0)
      (decf (current-completion mode))
      (echo "No previous completion.")))

(define-command scroll-completion-down
    (&optional (mode (find-submode 'active-completion-mode)))
  (setf (current-completion mode)
        (min (+ (current-completion mode) (scroll-distance mode))
             (1- (length (completions mode))))))

(define-command scroll-completion-up
    (&optional (mode (find-submode 'active-completion-mode)))
  (setf (current-completion mode)
        (max (- (current-completion mode) (scroll-distance mode))
             0)))

(define-command complete-selection
    (&optional (mode (find-submode 'active-completion-mode))
     (neomacs (find-submode 'neomacs-mode)))
  (bind ((selection (nth (current-completion mode) (completions mode)))
         ((text-pos . length) (replace-range mode)))
    (disable-modes* 'active-completion-mode (current-buffer))
    (with-dom-update neomacs
      (with-marker (marker text-pos)
        (insert-nodes text-pos (car selection))
        (delete-nodes marker length)))))

;;; Auto completion

(define-mode auto-completion-mode ()
  "Automatically show completion menu after input."
  ((minimum-prefix 3)
   (allowed-commands '(self-insert) :type (list-of symbol))))

(defun auto-completion-post-update ()
  (let ((mode (find-submode 'auto-completion-mode)))
    (when-let (node (symbol-around (focus)))
      (when (and
             (member (name (last-command *browser*))
                     (allowed-commands mode))
             (>= (length (text (first-child node)))
                 (minimum-prefix mode)))
        (show-completions (focus) t)))))

(defmethod enable ((mode auto-completion-mode) &key)
  (hooks:add-hook (post-command-hook (find-submode 'neomacs-mode))
                  'auto-completion-post-update))

(defmethod disable ((mode auto-completion-mode) &key)
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
