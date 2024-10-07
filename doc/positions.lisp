(in-package #:neomacs)

(spinneret:with-html
  (:nsection :title "Positions"
             (:p "A " (:i "position") " denotes somewhere in the DOM tree, which can be before or after some node, or between two adjacent nodes.")
             (:p "Positions may become invalid after editing operations. To maintain a valid position across arbitrary editing operations, see " (:a :href "markers.html" "Markers") ".")
             (:nsection :title "Types of positions"
                        (:ul
                         (:li "An " (:nxref :class-name 'element) " denotes the position before the " (:code "element") ".")
                         (:li (:code "(end-pos node)") " denotes the position at the end of " (:code "node") " (after any children). " (:code "node") " must be an " (:nxref :class-name 'element) ".")
                         (:li (:code "(text-pos node offset)") " denotes the position before the " (:code "offset") "-th character of " (:code "node") ". " (:code "node") " must be a " (:nxref :class-name 'text-node))
                         (:li (:code "nil") " denotes nowhere. Many position-related functions may return nil if requested position does not exist, and propagates nil if they receives nil position as an argument.")))
             (:nsection :title "Node around positions"
                        (:p "The following queries node around a given position. A node can be a " (:nxref :class-name 'character) " or " (:nxref :class-name 'element) ". If no node is found, nil is returned.")
                        (:ul
                         (:nxdoc :function 'node-after)
                         (:nxdoc :function 'node-before)
                         (:nxdoc :function 'node-containing)))
             (:nsection :title "Computing positions"
                        (:p "Basic position functions:")
                        (:ul
                         (:nxdoc :function 'pos-left)
                         (:nxdoc :function 'pos-right)
                         (:nxdoc :function 'pos-next)
                         (:nxdoc :function 'pos-prev)
                         (:nxdoc :function 'pos-up)
                         (:nxdoc :function 'pos-down)
                         (:nxdoc :function 'pos-down-last))
                        (:p "Iterate until or ensure a position predicate is satisfied:"
                            (:nxref :function 'pos-left-until) ", "
                            (:nxref :function 'pos-right-until) ", "
                            (:nxref :function 'pos-prev-until) ", "
                            (:nxref :function 'pos-next-until) ", "
                            (:nxref :function 'pos-up-until) ", "
                            (:nxref :function 'pos-left-ensure) ", "
                            (:nxref :function 'pos-right-ensure) ", "
                            (:nxref :function 'pos-prev-ensure) ", "
                            (:nxref :function 'pos-next-ensure) ", "
                            (:nxref :function 'pos-up-ensure) ".")
                        (:p "Destructive variants: "
                            (:nxref :function 'npos-left) ", "
                            (:nxref :function 'npos-right) ", "
                            (:nxref :function 'npos-next) ", "
                            (:nxref :function 'npos-prev) ", "
                            (:nxref :function 'npos-left-until) ", "
                            (:nxref :function 'npos-right-until) ", "
                            (:nxref :function 'npos-next-until) ", "
                            (:nxref :function 'npos-prev-until) ", "
                            (:nxref :function 'npos-left-ensure) ", "
                            (:nxref :function 'npos-right-ensure) ", "
                            (:nxref :function 'npos-next-ensure) ", "
                            (:nxref :function 'npos-prev-ensure) ".")
                        (:p "All of the above functions may take and return nil positions without signaling error."))
             (:nsection :title "Comparing positions"
                        (:p "Two positions point to the same location iff they are " (:nxref :function 'equalp) ".")
                        (:p "Additional functions for comparing positions:")
                        (:ul
                         (:nxdoc :function 'before-p)))))
