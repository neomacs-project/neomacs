(in-package #:neomacs)

(sera:export-always
    '(start))

(defun start (&optional (use-neomacs-debugger t))
  "Start the Neomacs system.

It's not safe to call this function more than once in a Lisp process.
If Neomacs system has been shut down (all frames are closed), restart
Lisp process before starting a new session.

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
  (unless ceramic.runtime:*releasep*
    (ceramic:setup)
    (copy-directory:copy
     (asdf:system-relative-pathname "neomacs" "assets/")
     (merge-pathnames #p"assets/"
                      (electron-tools:app-directory
                       (ceramic.file:release-directory)
                       :operating-system ceramic.os:*operating-system*))))
  (when ceramic.runtime:*releasep*
    (setf (logical-pathname-translations "sys")
          `(("SYS:SRC;**;*.*.*"
             ,(ceramic.runtime:executable-relative-pathname
               #P"src/sbcl/src/**/*.*"))
            ("SYS:CONTRIB;**;*.*.*"
             ,(ceramic.runtime:executable-relative-pathname
               #P"src/sbcl/contrib/**/*.*"))
            ("SYS:OUTPUT;**;*.*.*"
             ,(ceramic.runtime:executable-relative-pathname
               #P"src/sbcl/output/**/*.*")))))
  (write-string "!!! If Electron does't start up, it is likely that chrome-sandbox failed
!!! to start due to permission errors.

Try the following workaround:
1. sudo sysctl -w kernel.apparmor_restrict_unprivileged_userns=0
2. sudo sysctl kernel.unprivileged_userns_clone=1
3. sudo chown root electron/chrome-sandbox && sudo chmod 4755 electron/chrome-sandbox
")
  (ceramic:start)
  (mount-asset "sys" (ceramic:resource-directory 'assets))
  (mount-asset "user" (uiop:xdg-config-home "neomacs/assets/"))
  (setf *current-frame-root* (make-frame)
        *use-neomacs-debugger* use-neomacs-debugger)
  (start-command-loop)
  (let ((config-file (uiop:xdg-config-home "neomacs" "init.lisp")))
    (if (uiop:file-exists-p config-file)
        (progn
          (format t "Loading ~a.~%" config-file)
          (load config-file))
        (format t "~a not yet exist.~%" config-file)))
  (load-web-history))

(in-package #:ceramic-entry)

(defun neomacs ()
  (let ((ceramic.runtime:*releasep* t))
    (neomacs::start)
    (sb-ext:process-wait
     (slot-value ceramic.driver:*driver* 'ceramic.driver::process))
    (neomacs::kill-neomacs)))
