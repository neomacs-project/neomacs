(in-package #:neomacs)

(sera:export-always
    '(start *store-path* *startup-hooks* *kill-hooks*))

(defvar *store-path* nil
  "Path to bknr.datastore, defaults to (uiop:xdg-data-home \"neomacs/store/\").")

(defvar *startup-hooks* '(install-adblocker mount-pdf-tmp)
  "List of functions to run after Neomacs starts.")

(defvar *kill-hooks* nil
  "List of functions to run before `kill-neomacs'.")

(defun start (&optional (self-govern t))
  "Start the Neomacs system.

It's not safe to call this function more than once in a Lisp process.
If Neomacs system has been shut down (all frames are closed), restart
Lisp process before starting a new session.

If SELF-GOVERN is nil, Neomacs assumes it is being started
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
    (with-open-file
        (s (merge-pathnames
            #p"resources/default_app/package.json"
            (ceramic.file:release-directory))
           :direction :output
           :if-exists :supersede)
      (write-string "{ \"name\": \"Neomacs\", \"version\": \"0.1.0\", \"main\": \"main.js\" }" s)))
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
  (setq *contents-tmp-path*
        (merge-pathnames #p"neomacs/contents/" (uiop:temporary-directory)))
  (ensure-directories-exist *contents-tmp-path*)
  (mount-asset "contents" *contents-tmp-path*)
  (let ((intro-path (ceramic:resource 'doc "build/intro.html")))
    (when (uiop:file-exists-p intro-path)
      (make-buffer
       "*intro*" :mode 'web-mode
       :url (file-path-url intro-path))))
  (setf *current-frame-root* (make-frame))
  (unless self-govern
    (setf *no-debugger* t
          *no-redirect-stream* t))
  (unless (get-buffer "*scratch*") (make-scratch))
  (start-command-loop)
  (unless *no-init*
    (let ((*package* (find-package "NEOMACS-USER"))
          (config-file (uiop:xdg-config-home "neomacs" "init.lisp")))
      (if (uiop:file-exists-p config-file)
          (progn
            (format t "Loading ~a.~%" config-file)
            (load config-file))
          (format t "~a not yet exist.~%" config-file))))
  (evaluate-javascript
   (format nil "Ceramic.downloadPath = ~S"
           (quote-js (uiop:native-namestring "~/Downloads")))
   :global)
  (unless *store-path*
    (setq *store-path* (uiop:xdg-data-home "neomacs/store/")))
  (make-instance 'bknr.datastore:mp-store
                 :directory *store-path*
                 :subsystems (list (make-instance 'bknr.datastore:store-object-subsystem)))
  (dolist (h *startup-hooks*)
    (with-demoted-errors (format nil "Error running startup hook ~a: " h)
      (funcall h)))
  ;; If we are started from terminal instead of SLIME, don't let
  ;; `start' return, otherwise SBCL top-level tries to read from
  ;; Neomacs input stream and cause funny
  (when (and (not *no-redirect-stream*)
             (eql *standard-input* *current-standard-input*))
    (bt:join-thread *command-loop-thread*)))


(define-command kill-neomacs ()
  "Exit Neomacs."
  (dolist (h *kill-hooks*)
    (with-demoted-errors (format nil "Error running kill hook ~a: " h)
      (funcall h)))
  ;; Mark all buffer as non-alive to suppress post-command operations
  (clrhash *buffer-table*)
  (sb-concurrency:send-message *event-queue* 'quit)
  (ceramic:quit))

(define-keys :global
  "C-x C-c" 'kill-neomacs)
