(uiop:define-package neomacs
  (:use #:common-lisp #:nyxt #:array-utils #:lwcells #:named-closure #:iter)
  (:export #:neomacs) (:shadow #:class)
  (:import-from #:serapeum #:lret #:lret* #:lastcar #:single #:only-elt #:eval-always)
  (:import-from #:bind #:bind)
  (:import-from #:trivia #:ematch #:match)
  (:import-from #:alexandria #:if-let #:when-let #:first-elt #:last-elt))

(in-package :neomacs)
(use-nyxt-package-nicknames)
