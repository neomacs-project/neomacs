(in-package #:neomacs)

(defun start (&optional (use-neomacs-debugger t))
  "Start the Neomacs system.

If USE-NEOMACS-DEBUGGER is nil, Neomacs assumes it is being started
from an external Lisp development environment (e.g. SLIME). This has
the following effect:

- The Neomacs debugger is disabled. Errors are to be handled by the
  external IDE.

- The command loop does not call `setup-stream-indirection'. Standard
  input/output streams are provided by the external IDE."
  ;; We don't have preemptive quit yet, so we put in those to avoid
  ;; infinite recursion
  (setq *print-level* 50 *print-length* 50)
  (let ((config-file (uiop:xdg-config-home "neomacs" "init.lisp")))
    (if (uiop:file-exists-p config-file)
        (progn
          (format t "Loading ~a.~%" config-file)
          (load config-file))
        (format t "~a not yet exist.~%" config-file)))
  (load-web-history)
  (unless ceramic.runtime:*releasep*
    (ceramic:setup)
    (copy-directory:copy
     (asdf:system-relative-pathname "neomacs" "assets/")
     (merge-pathnames #p"assets/"
                      (electron-tools:app-directory
                       (ceramic.file:release-directory)
                       :operating-system ceramic.os:*operating-system*))))
  (ceramic:start)
  (evaluate-javascript
   (format nil "Mounts['sys']=`~a`"
           (quote-js
            (uiop:native-namestring
             (ceramic:resource-directory 'assets))))
   :global)
  (setf *current-frame-root* (make-frame-root (make-scratch))
        *use-neomacs-debugger* use-neomacs-debugger)
  (start-command-loop))

(in-package #:ceramic-entry)

(defun neomacs ()
  (let ((ceramic.runtime:*releasep* t))
    (neomacs::start)
    (sb-thread:join-thread neomacs::*command-loop-thread*)))
