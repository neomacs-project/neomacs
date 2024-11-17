(defpackage neomacs
  (:use #:common-lisp #:lwcells #:named-closure #:iter)
  (:export #:neomacs) (:shadow #:class)
  (:import-from #:serapeum #:lret #:lret* #:lastcar #:single #:only-elt #:eval-always)
  (:import-from #:bind #:bind)
  (:import-from #:trivia #:ematch #:match)
  (:import-from #:alexandria #:if-let #:when-let #:when-let*
                #:first-elt #:last-elt #:assoc-value))
(in-package #:neomacs)
(trivial-package-local-nicknames:add-package-local-nickname
 '#:sera '#:serapeum)
(trivial-package-local-nicknames:add-package-local-nickname
 '#:alex '#:alexandria)
(trivial-package-local-nicknames:add-package-local-nickname
 '#:cera.d '#:ceramic.driver)
