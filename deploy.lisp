(in-package #:neomacs)

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h :long "help")
  (:name :no-debugger
   :description "turn off Neomacs debugger"
   :long "no-debugger")
  (:name :no-redirect
   :description "turn off stream redirection (don't capture stdin/out)"
   :long "no-redirect")
  (:name :no-init
   :description "don't load init.lisp"
   :short #\q :long "no-init")
  (:name :dev
   :description "dev mode, same as --no-debugger --no-redirect"
   :short #\d :long "dev")
  (:name :debug-init
   :description "turn on *debug-on-error* when loading init.lisp"
   :long "debug-init"))

(defun entry ()
  (handler-case
      (let ((opts (opts:get-opts)))
        (when (getf opts :help)
          (opts:describe)
          (opts:exit 0))
        (setf *no-debugger* (getf opts :no-debugger))
        (setf *no-redirect-stream* (getf opts :no-redirect))
        (setf *no-init* (getf opts :no-init))
        (when (getf opts :dev)
          (setf *no-debugger* t *no-redirect-stream* t))
        (setf *debug-on-error* (getf opts :debug-init)))
    (error (c)
      (format t "~a~%" c)
      (opts:describe)
      (opts:exit 2)))
  (setq ceramic.runtime:*releasep* t)
  (neomacs::start)
  (sb-ext:process-wait
   (slot-value ceramic.driver:*driver* 'ceramic.driver::process))
  (neomacs::kill-neomacs))

(deploy:define-hook (:deploy neomacs) (directory)
  (copy-directory:copy
   (ceramic.file:release-directory)
   (merge-pathnames #p"electron/" directory))
  (trivial-exe:ensure-executable
   (ceramic.bundler::binary-pathname
    (merge-pathnames #p"electron/" directory)
    :operating-system ceramic.bundler::*operating-system*))
  (ceramic.resource:copy-resources
   (merge-pathnames #p"resources/" directory))
  (asdf-bundler:copy-dependencies
   "neomacs" (merge-pathnames #p"src/" directory))
  (ensure-directories-exist
   (merge-pathnames #p"src/neomacs/" directory))
  (iter (for path in (uiop:directory-files (asdf:system-source-directory "neomacs")))
    (uiop:copy-file
     path
     (make-pathname
      :name (pathname-name path)
      :type (pathname-type path)
      :defaults (merge-pathnames #p"src/neomacs/" directory))))
  (copy-directory:copy (asdf:system-relative-pathname "neomacs" "modes/")
                       (merge-pathnames #p"src/neomacs/modes/" directory))
  (copy-directory:copy (asdf:system-relative-pathname "neomacs" "term/")
                       (merge-pathnames #p"src/neomacs/modes/" directory))
  (copy-directory:copy (translate-logical-pathname "sys:src;")
                       (merge-pathnames #p"src/sbcl/src/" directory))
  (copy-directory:copy (translate-logical-pathname "sys:contrib;")
                       (merge-pathnames #p"src/sbcl/contrib/" directory))
  (copy-directory:copy (translate-logical-pathname "sys:output;")
                       (merge-pathnames #p"src/sbcl/output/" directory))
  (uiop:delete-file-if-exists
   (merge-pathnames #p"electron/resources/default_app.asar" directory))
  (uiop:run-program (list "asar" "pack"
                          (uiop:native-namestring
                           (merge-pathnames #p"electron/resources/default_app/" directory))
                          (uiop:native-namestring
                           (merge-pathnames #p"electron/resources/default_app.asar" directory)))))
