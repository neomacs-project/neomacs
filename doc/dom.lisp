(in-package #:neomacs)

(spinneret:with-html
  (:nsection :title "Reactive DOM"
            (:p "Neomacs maintains reactive DOMs based on " (:nxref :package :lwcells) ". This enables observers and computed attributes to update in real-time depending on DOM content.")
            (:nsection :title "Nodes"
                       (:p "This section documents the low-level node classes making up Neomacs's reactive DOM. Note that the interface here is low-level in the sense that " (:nxref :class-name 'text-node) "s are being exposed. The majority of Neomacs API hides " (:nxref :class-name 'text-node) " as an implementation detail and the " (:i "conceptual DOM" ) " consists of "(:nxref :class-name 'element) " and " (:nxref :class-name 'character)  ".")
                       (:ul
                        (:li (:nxref :class-name 'node))
                        (:li (:nxref :class-name 'text-node))
                        (:li (:nxref :class-name 'element))
                        (:nxdoc :function 'child-nodes)
                        (:nxdoc :function 'element-p)
                        (:nxdoc :function 'text-node-p)
                        (:nxdoc :function 'clone-node)))
            (:nsection :title "Traversing DOM"
                       (:ul
                        (:nxdoc :function 'do-dom)
                        (:nxdoc :function 'do-elements)))
            (:nsection :title "Attributes"
                       (:ul
                        (:nxdoc :function 'attribute)
                        (:nxdoc :function 'set-attribute-function)))
            (:nsection :title "Low-level DOM edits"
                       (:p "This section documents low-level primitives for modifying Lisp-side DOM. They are used to implement programmer-facing editing operations, see " (:a :href "edit.html#editing-primitives" "Editing primitives") ".")
                       (:ul
                        (:nxdoc :function 'insert-before)
                        (:nxdoc :function 'append-child)
                        (:nxdoc :function 'append-children)
                        (:nxdoc :function 'remove-node)))))
