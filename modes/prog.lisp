(in-package #:neomacs)

(define-mode prog-mode (doc-mode)
  ((completion-limit
    :default 300
    :documentation
    "How many best auto-completion items to compute.

This is a suggestive value for auto-completion backend."))
  (:hooks undo-mode))
