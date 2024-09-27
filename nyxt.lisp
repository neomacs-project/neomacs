(ql:quickload "nyxt/gi-gtk")
(ql:quickload "neomacs")

(in-package :nyxt-user)

(define-configuration buffer
  ((default-modes
    (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration base-mode
  ((keyscheme-map
    (let ((m %slot-default%))
      (define-key (gethash nyxt/keyscheme:emacs m) "C-x C-f" 'neomacs:open-file)
      m))))

(defun k-init (b)
  (declare (ignore b))
  (start-swank)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(define-configuration browser
  ((after-init-hook
    (hooks:add-hook %slot-value% 'k-init))))

(sb-int:with-float-traps-masked (:divide-by-zero)
  (nyxt:start))
