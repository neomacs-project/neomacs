(in-package #:neomacs)

(define-mode file-mode ()
  "Generic mode for buffer backed by files."
  (filename
   (keyscheme-map
    (keymaps:define-keyscheme-map "file" ()
      keyscheme:emacs
      '("C-x C-s" save-buffer))))
  (:toggler-command-p nil))

(defgeneric load-contents (file-mode)
  (:method :after ((mode t))
    (let ((neomacs (current-neomacs)))
      (dolist (c (child-nodes *dom-output*))
        (do-dom (alex:rcurry #'node-setup neomacs) c)))))

(defvar *dom-output* nil)

(defun call-with-dom-output (body)
  (if *dom-output* (funcall body)
      (let ((*dom-output* (make-instance 'element :tag-name "div")))
        (funcall body)
        (child-nodes *dom-output*))))

(defgeneric write-file (file-mode))

(define-command save-buffer
    (&optional (mode (find-submode 'file-mode)))
  (write-file mode)
  (echo "Wrote ~a" (filename mode)))

(define-internal-scheme "neomacs"
    (lambda (url)
      (let ((mode (current-neomacs))
            (filename (quri.uri:uri-path (quri:uri url))))
        (setf (history-file (current-buffer))
              (make-instance 'history-file
                             :profile (make-instance 'nofile-profile)))
        (spinneret:with-html-string
          (:head)
          (:body
           (:raw
            (let* ((doc-node (make-instance 'element :tag-name "div"))
                   (*inhibit-dom-update* t))
              (setf (attribute doc-node "class") "doc")
              (assign-neomacs-id doc-node)
              (setf (host doc-node) mode
                    (focus-marker mode)
                    (make-instance 'marker :pos (end-pos doc-node))
                    (selection-marker mode)
                    (make-instance 'marker :pos (end-pos doc-node))
                    (restriction mode) doc-node)
              (when filename
                (let ((*dom-output* doc-node)
                      (mode (find-submode 'file-mode)))
                  (setf (filename mode) filename)
                  (load-contents mode)))
              (serialize doc-node nil)))
           (:div :id "neomacs-highlight" :style "display: none")
           (:table (:tbody :id "completion-menu" :style "display: none")))))))

(define-command-global open-file
    (&optional
     (filename
      (uiop:native-namestring
       (pathname
        (prompt1
         :prompt "Open file"
         :extra-modes 'nyxt/mode/file-manager:file-manager-mode
         :input (uiop:native-namestring (uiop:getcwd))
         :sources
         (list (make-instance 'nyxt/mode/file-manager:file-source
                              :name "Existing file"
                              :actions-on-return #'identity)
               (make-instance 'prompter:raw-source
                              :name "Create new file")))))))
  "Open a new Neomacs buffer and query a FILE to edit in it."
  (set-current-buffer
   (make-buffer :url (quri:make-uri :scheme "neomacs" :path filename))))
