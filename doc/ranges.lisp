(in-package #:neomacs)

(spinneret:with-html
  (:nsection :title "Ranges"
             (:nsection :title "Range operations"
                        (:ul
                         (:nxdoc :function 'range)
                         (:nxdoc :function 'range-collapsed-p)
                         (:nxdoc :function 'inside-range-p)
                         (:nxdoc :function 'extract-range)
                         (:nxdoc :function 'delete-range)))
             (:nsection :title "Range selection")))
