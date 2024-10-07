(in-package #:neomacs)

(spinneret:with-html
  (:nsection :title "Undo"
             (:p "The following functions maintains the undo history.")
             (:ul
              (:nxdoc :function 'record-undo)
              (:nxdoc :variable '*inhibit-record-undo*)
              (:nxdoc :function 'undo-auto-amalgamate)
              (:nxdoc :function 'undo-boundary))
             (:p "The following functions and commands perform undo and redo operations.")
             (:ul
              (:nxdoc :function 'undo)
              (:nxdoc :function 'redo)
              (:nxdoc :function 'undo-command)
              (:nxdoc :function 'redo-command))))
