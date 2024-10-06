(in-package #:neomacs)

(define-class mode (default-mixin)
  (keymap))

(defgeneric enable-aux (mode-name)
  (:method ((mode-name symbol))))

(defgeneric disable-aux (mode-name)
  (:method ((mode-name symbol))))
