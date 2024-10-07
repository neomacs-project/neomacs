(in-package #:neomacs)

(spinneret:with-html
  (:nsection :title "Motion"
             (:nsection :title "Selectable positions")
             (:nsection :title "Motion commands"
                        (:p "The following moves according to structure of the document.")
                        (:ul
                         (:nxdoc :command 'forward-node :mode 'neomacs-mode)
                         (:nxdoc :command 'backward-node :mode 'neomacs-mode)
                         (:nxdoc :command 'forward-element :mode 'neomacs-mode)
                         (:nxdoc :command 'backward-element :mode 'neomacs-mode)
                         (:nxdoc :command 'backward-up-node :mode 'neomacs-mode)
                         (:nxdoc :command 'beginning-of-buffer :mode 'neomacs-mode)
                         (:nxdoc :command 'end-of-buffer :mode 'neomacs-mode)
                         (:nxdoc :command 'forward-word :mode 'neomacs-mode)
                         (:nxdoc :command 'backward-word :mode 'neomacs-mode)
                         (:nxdoc :command 'beginning-of-line :mode 'neomacs-mode)
                         (:nxdoc :command 'end-of-line :mode 'neomacs-mode)
                         (:nxdoc :command 'beginning-of-defun :mode 'neomacs-mode)
                         (:nxdoc :command 'end-of-defun :mode 'neomacs-mode))
                        (:p "The following line motion commands try to keep horizontal location approximately the same. Currently, we do this by counting number of selectable positions between current focus position and beginning-of-line position, and try to keep that number the same.")
                        (:ul
                         (:nxdoc :command 'previous-line :mode 'neomacs-mode)
                         (:nxdoc :command 'next-line :mode 'neomacs-mode)
                         (:nxdoc :command 'scroll-up-command :mode 'neomacs-mode)
                         (:nxdoc :command 'scroll-down-command :mode 'neomacs-mode)))))
