(in-package #:neomacs)

(define-class mode (default-mixin)
  (keymap))

(defmacro define-mode (name super-classes slots &rest options)
  `(define-class ,name (,@super-classes mode) ,slots ,@options))

(defun find-submode (name &optional (buffer (current-buffer)))
  (find-if (lambda (m) (typep m name)) (modes buffer)))

(defun mode-name (mode-instance)
  (class-name (class-of mode-instance)))

(defgeneric enable (mode)
  (:method :before ((mode mode))
    (when (find-submode (mode-name mode))
      (error "~A already enabled." (mode-name mode)))
    (push mode (modes (current-buffer))))
  (:method ((mode-name symbol))
    (enable (make-instance mode-name)))
  (:method ((mode mode))))

(defgeneric disable (mode)
  (:method :after ((mode mode))
    (alex:deletef (modes (current-buffer)) mode))
  (:method ((mode-name symbol))
    (if-let (mode (find-submode mode-name))
      (disable mode)
      (error "~A not enabled." mode-name)))
  (:method ((mode mode))))

(defun ensure (mode-symbol)
  (unless (find-submode mode-symbol)
    (enable mode-symbol)))
