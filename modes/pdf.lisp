(in-package #:neomacs)

(defvar *pdf-tmp-path* nil)

(defun mount-pdf-tmp ()
  (setq *pdf-tmp-path* (merge-pathnames #p"neomacs/pdf/" (uiop:temporary-directory)))
  (mount-asset "pdf.contents" *pdf-tmp-path*))

(pushnew '("pdf" pdf-mode) *file-type-hooks* :test 'equal)

(define-mode pdf-mode (web-mode read-only-mode)
  ((file-path))
  (:lighter "PDF")
  (:documentation "Mode for viewing PDF file using mupdf."))

(defun pdf-output-path (buffer page)
  (merge-pathnames
   (format nil "~a/~a.svg" (id buffer) page)
   *pdf-tmp-path*))

(defun pdf-html-path (buffer)
  (merge-pathnames
   (format nil "~a/index.html" (id buffer))
   *pdf-tmp-path*))

(defun pdf-ensure-page (buffer page)
  (unless (uiop:file-exists-p (pdf-output-path buffer page))
    (let ((output-template
            (merge-pathnames
             (format nil "~a/%d.svg" (id buffer))
             *pdf-tmp-path*)))
      (ensure-directories-exist output-template)
      (uiop:run-program
       (list "mutool" "draw" "-o"
             (uiop:native-namestring output-template)
             (uiop:native-namestring (file-path buffer))
             (format nil "~a" page))
       :output t :error t))))

(setf (gethash "pdf-page-request" *ipc-handler-table*)
      (lambda (buffer details)
        (let ((page (assoc-value details :pagenum)))
          (pdf-ensure-page buffer page)
          (evaluate-javascript
           (format nil "document.getElementById('page-~a').src='~a.svg'" page page)
           buffer))))

(setf (gethash "pdf-buffer" *ipc-handler-table*)
      (lambda (buffer details)
        (let ((src (assoc-value details :src)))
          (with-current-buffer buffer
            (enable 'pdf-mode)
            (setf (file-path buffer)
                  (merge-pathnames
                   (format nil "~a.pdf" (id buffer))
                   *pdf-tmp-path*))
            (ensure-directories-exist (file-path buffer))
            (alex:write-byte-vector-into-file
             (dex:get src :force-binary t) (file-path buffer) :if-exists :supersede)
            (revert-buffer)))))

(defun pdf-render-html (buffer)
  (let ((pages (plump:parse
                (uiop:run-program
                 (list "mutool" "pages" (uiop:native-namestring (file-path buffer)))
                 :output :string :error t)))
        (output-path (pdf-html-path buffer)))
    (ensure-directories-exist output-path)
    (with-open-file (s output-path :direction :output :if-exists :supersede)
      (serialize-document
       (dom `(:body
              ,@ (iter (for page in-vector (plump:child-elements pages))
                   (for crop-box = (or (car (plump-dom:get-elements-by-tag-name page "CropBox"))
                                       (car (plump-dom:get-elements-by-tag-name page "MediaBox"))
                                       (error "Malformed PDF: no MediaBox")))
                   (for pagenum = (plump:get-attribute page "pagenum"))
                   (collecting
                     `(:img :loading "lazy"
                            :src ,(str:concat pagenum ".svg")
                            :id ,(str:concat "page-" pagenum)
                            :onerror ,(format nil "electronAPI.send('neomacs',{type:'pdf-page-request',pagenum:~a})" pagenum)
                            :width  ,(plump:get-attribute crop-box "r")
                            :height  ,(plump:get-attribute crop-box "t"))))))
       (styles (current-buffer)) s))))

(defmethod revert-buffer-aux :around ((buffer pdf-mode))
  (disable 'file-mode)
  (uiop:delete-directory-tree
   (make-pathname :name nil :type nil :version nil :defaults (pdf-html-path buffer))
   :validate t :if-does-not-exist :ignore)
  (pdf-render-html buffer)
  (setf (url buffer) (str:concat (file-path-url (pdf-html-path buffer))))
  (call-next-method))

(defsheet pdf-mode `(("img"
                      :margin "0 auto"
                      :display "block"
                      :background-color "#fff")))
