(in-package #:neomacs)

(spinneret:with-html
  (:nsection :title "Markers"
             (:p "A " (:nxref :class-name 'marker) " maintains a position in the buffer, which stays valid across arbitrary editing operations.")
             (:nsection :title "Marker advance types"
                        (:ul
                         (:p "When an insertion happens at a marker, the marker may get pushed after the inserted contents (it " (:i "advances") "), or stay before the inserted contents (it does not advance). This property can be queried and set using the following function:")
                         (:nxdoc :function 'advance-p)))))
