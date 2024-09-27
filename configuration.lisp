(in-package #:neomacs)

(defun add-mode-hook (trigger-mode toggle-mode)
  (hooks:add-hook
   (nyxt::customize-hook (find-class trigger-mode))
   (make-instance 'hooks:handler
    :name toggle-mode
    :fn (lambda (object)
          (hooks:add-hook
           (enable-hook object)
           (make-instance
            'hooks:handler
            :name toggle-mode
            :fn (lambda (mode)
                  (enable-modes* toggle-mode (buffer mode)))))
          (hooks:add-hook
           (disable-hook object)
           (make-instance
            'hooks:handler
            :name toggle-mode
            :fn (lambda (mode)
                  (disable-modes* toggle-mode (buffer mode))))))))
  toggle-mode)

(defun remove-mode-hook (trigger-mode toggle-mode)
  (hooks:remove-hook
   (nyxt::customize-hook (find-class trigger-mode))
   toggle-mode)
  toggle-mode)
