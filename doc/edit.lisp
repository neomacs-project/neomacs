(in-package #:neomacs)

(spinneret:with-html
  (:nsection :title "Editing"
        (:nsection :title "Editing primitives"
          (:p "Lisp programs are expected to use the following primitives to edit the Neomacs DOM. These primitives provides " (:a :href "positions.html" "Positions")  "-based interface and handles " (:nxref :class-name 'text-node) " splitting/merging automatically. These primitives also maintain " (:a :href "undo.html" "Undo") " history, updates browser renderer-side DOM, setup and destruction of observers and computed attributes, and allocation of neomacs-identifier.")
          (:ul
           (:nxdoc :function 'delete-nodes)
           (:nxdoc :function 'extract-nodes)
           (:nxdoc :function 'insert-nodes)
           (:nxdoc :function 'move-nodes)))
        (:nsection :title "Compound editing operations"
          (:ul
           (:nxdoc :function 'splice-node)
           (:nxdoc :function 'join-nodes)
           (:nxdoc :function 'raise-node)
           (:nxdoc :function 'split-node)))
        (:nsection :title "Editing commands"
          (:ul
           (:nxdoc :command 'new-line :mode 'neomacs-mode)
           (:nxdoc :command 'backward-delete :mode 'neomacs-mode)
           (:nxdoc :command 'forward-delete :mode 'neomacs-mode)
           (:nxdoc :command 'backward-cut-word :mode 'neomacs-mode)
           (:nxdoc :command 'cut-element :mode 'neomacs-mode)
           (:nxdoc :command 'copy-element :mode 'neomacs-mode)
           (:nxdoc :command 'paste :mode 'neomacs-mode)
           (:nxdoc :command 'paste-pop :mode 'neomacs-mode)
           (:nxdoc :command 'forward-cut :mode 'neomacs-mode)))))
