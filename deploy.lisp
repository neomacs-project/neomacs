(in-package #:neomacs)

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
