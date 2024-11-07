(in-package #:neomacs)

(define-mode prog-mode (doc-mode)
  ((completion-limit
    :default 300
    :documentation
    "How many best auto-completion items to compute.

This is a suggestive value for auto-completion backend.")
   (completion-minimum-prefix
    :default 3
    :documentation
    "Compute auto completion only for prefix longer than this value.

This is a suggestive value for auto-completion backend."))
  (:hooks undo-mode))
